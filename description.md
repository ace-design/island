---
layout: default
title: Game description
---

# Island Game Description


The player acts as the manager of an exploration raid. The raid is funded by a _shipowner_ named Ephron Vestrit,  who is a well renowned trader operating from a place called Bingtown (<sup>[1](#myfootnote1)</sup>). Ephron is negotiating an important contract with a customer for selling some resources at a very good price. In the meanwhile, he gives you the command of his best assets and mens to collect the needed resources, supposedly available on an unexplored island. The deal is simple: 

  * You must be back in Bingtown before the end of the negotiation, or Ephron will loose credits and look like a fool;
  * You must take care of the assets provided by Ephron;
  * You must collect the asked resources so that Ephron can sell it to his customer (incomplete quantities will not be tolerated)




## Context initialization

At the beginning of the game, you receive an assignment from Ephron, that is, the location of the island, the deadline of the deal, the number of mens available as support for this mission, the resources he intents to sell to his customer and the necessary information to start the aerial phase. This assignment is modeled as a JSON data structure (be careful, ordering is not relevant in JSON structure).

```json
{ 
  "men": 12,
  "budget": 10000,
  "contracts": [
    { "amount": 600, "resource": "WOOD" },
    { "amount": 200, "resource": "GLASS" }
  ],
  "heading": "W"
}
```

In the previous example, Ephron is giving you 12 mens for this raid. You have 10,000 action points to spend at most in this raid, spending more will fail the deal. The assignment contains two contracts (Ephron will always give you at least one contract). You have to bring back from the island _(i)_ 600 units of wood and _(ii)_ 200 units of glass. For the aerial phase, you are initially facing west.

Obviously, exploring an unknown island is a risky business. Ephron do prefer the team to be safely back in Bingtown instead of missing in action. It might also happen that the island does not contain the resources he was expecting, or in too low quantities. In this case, the raid must give up and come back as soon as possible to inform him of the situation.

## Biome geographical model

Islands are modeled according to non-stupid geographical choices. However the generator does not ensure to always produce _valid_ island with respect to classical Earth standards. The map is covered by _biomes_, representing formation of plants and animals with consistent characteristics. In standard geography, biome repartition is modeled as a Whittaker diagrams. For a given location on Earth, a function of the average temperature and annual precipitation amount identifies the associated biome. The following figure ([source](http://w3.marietta.edu/~biol/biomes/biome_main.htm)) describes such a diagram:

<div align="center">
  <img src="{{ site.baseurl }}assets/whittaker.jpg" />
</div>

The Island game model does not implement temperature and annual precipitation values. We mapped those values to _(i)_ altitude for temperature (the higher you are, the colder it is) and _(ii)_ humidity for annual precipitations. This is a gross approximation, but it is good enough for the  map generator. 

The available diagrams used by the engine are defined in its standard library: [WhittakerDiagrams.scala](https://github.com/ace-design/island/blob/develop/engine/src/main/scala/eu/ace_design/island/stdlib/WhittakerDiagrams.scala)


### List of the 17 available biomes

For each biome, a short description and the `code` the game engine returns when you encounter such a biome on a map.

  * Common biomes:
    * Ocean (`OCEAN`): plain ocean, wide open area full of unknown;
    * Lake (`LAKE`): internal lake, potentially big, with freshwater;
    * Beach (`BEACH`): beach (not always sandy);
    * Grassland (`GRASSLAND`): area of prairie;
  * Tropical biomes:
    * Mangrove (`MANGROVE`): super wet area, home of the mangle tree;
    * Tropical rain forest (`TROPICAL_RAIN_FOREST`): hot and wet;
    * Tropical seasonal forest (`TROPICAL_SEASONAL_FOREST`): less wet, but not less hot;
  * Temperate biomes
    * Temperate deciduous forest (`TEMPERATE_DECIDUOUS_FOREST`): classical forests with trees that lose their leaves each year;
    * Temperate rain forest (`TEMPERATE_RAIN_FOREST`): very rare biome, very wet area coupled to low temperatures;
    * Temperate desert (`TEMPERATE_DESERT`): aride area with sparse vegetation and very few humidity;
  * Nordic / Mountain Biomes
    * Taiga (`TAIGA `): boreal forest, cold and majestuous;
    * Snow (`SNOW`): area covered with snow, wet and cold;
    * Tundra (`TUNDRA`): arctic prairie, surviving on permanently frozen soil;
    * Alpine (`ALPINE`): rocky mountains, not always covered by snow;
    * Glacier (`GLACIER`): inhospitable area, full of ice;
  * Subtropical biomes
    * Shrubland (`SHRUBLAND`): prairie dominated by shrubs, such as _maquis_ in Corsica or _garrigue_ in Provence;
    * Subtropical desert (`SUB_TROPICAL_DESERT`) very dry and inhospitable area

The following picture gives an overview of the different biomes ([sources](https://gist.github.com/mosser/cc787790d4137cec7677))

<div align="center">
  <img src="{{ site.baseurl }}assets/panorama.png" />
</div>

## Resource descriptions

The island contains primary resources, _i.e._, resources one can exploit from the biomes available on the island. The raiders can also craft manufactured resource using primary ones.

### Primary resources

<div align="center">
	<table style="text-align: center">
		<tr><th>Resource</th><th>Description</th><th>Produced by</th></tr>
		<tr>
			<td><code>FISH</code></td>
			<td>fresh fish from water areas</td>
			<td>oceans and lakes</td>
		</tr>
		<tr>
			<td><code>FLOWER</code></td>
			<td>different species of very rare aromatic flowers</td>
			<td>alpine, glacier and mangrove</td>
		</tr>
		<tr>
			<td><code>FRUITS</code></td>
			<td>exotic fruits from tropical regions</td>
			<td>tropical forests</td>
		</tr>
		<tr>
			<td><code>FUR</code></td>
			<td>obtained from games</td>
			<td>prairies and temperate rainforest</td>
		</tr>
		<tr>
			<td><code>ORE</code></td>
			<td>mined resource</td>
			<td>alpine regions and deserts</td>
		</tr>
		<tr>
			<td><code>QUARTZ</code></td>
			<td>silicium cristal</td>
			<td>beaches and deserts</td>
		</tr>
		<tr>
			<td><code>SUGAR_CANE</code></td>
			<td>flavored plant with high economic value</td>
			<td>tropical forests</td>
		</tr>
		<tr>
			<td><code>WOOD</code></td>
			<td>trunks and logs of solid wood</td>
			<td>mangroves and forests</td>
		</tr>
	</table>
</div>


### Manufactured resources

Manufactured resources are produced on the campground, transforming the already collected resources into more valuable goods. It is important to notice that some of the manufactured resources recipes are not influenced by the number of mens in the raid (<sup>[2](#myfootnote2)</sup>). This is related to the limited resources associated to the crafting of such a manufactured resource. Ingots creation is limited by the capacity of the kiln available at the campground. Rum distilling is a time-consuming process, where the time given to the preparation is more important than the amount of workers.

<div align="center">
	<table style="text-align: center">
		<tr><th>Resource</th><th>Description</th><th>Recipe (subject to variations)</th></tr>
		<tr>
			<td><code>GLASS</code></td>
			<td>fine glass jewelry</td>
			<td> 5 quartz + 10 woods ~> 1 glass </td>
		</tr>
		<tr>
			<td><code>INGOT</code></td>
			<td>standardized ingots</td>
			<td> 5 ores + 5 woods ~> 1 ingot </td>
		</tr>
		<tr>
			<td><code>LEATHER</code></td>
			<td>high quality leather pieces</td>
			<td> 3 furs ~> 1 leather </td>
		</tr>
		<tr>
			<td><code>PLANK</code></td>
			<td>materials for carpenters</td>
			<td> 1 wood ~> 4 planks </td>
		</tr>
		<tr>
			<td><code>RUM</code></td>
			<td>world renowned beverage</td>
			<td>10 sugar canes + 1 fruit ~> 1 rum</td>
		</tr>
	</table>
</div>


## Notes

<a name="myfootnote1">1</a>: The background folklore is extracted from the awesome _Liveship Traders Trilogy_, by Robin Hobb. Without the dragons (at least at the beginning).

<a name="myfootnote2">2</a>: as well as you'll not obtain a baby in one month by using 9 mens in the process.