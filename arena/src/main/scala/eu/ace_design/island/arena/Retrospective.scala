package eu.ace_design.island.arena

import java.io.{File, PrintWriter}

import eu.ace_design.island.arena.exporters.GameLogExporter
import org.json.{JSONArray, JSONObject}
import eu.ace_design.island.arena.utils._
import eu.ace_design.island.map.resources.Resource
import org.json


trait Retrospective extends Teams with App {

  val weeks: Set[Run]
  val outputDir: String

  def trigger(): Unit = {

    val start = System.currentTimeMillis()
    val jobs = asJobs(weeks)
    val runner = Runner(outputDir = outputDir, exporters = Seq(classOf[GameLogExporter]))
    val results = runner(asPlayers, jobs)
    val stop = System.currentTimeMillis()
    val delta = stop - start

    displayJobs(jobs)
    export2json(s"$outputDir/_results.json", jobs, results)
    display(delta, results, jobs)
  }

  protected def asJobs(runs: Set[Run]): Set[Job] = {
    runs map { _.asJob }
  }

  private def display(delta: Long, results: Set[Result], jobs: Set[Job]): Unit = {
    println("# Retrospective Results\n")

    val minutes = delta.toDouble / 1000 / 60

    println(f"  - Execution time: $minutes%.2f minutes (${delta}ms)\n")
    val avail = (jobs.toSeq flatMap { j => j.contract.objectives.toSeq }).size
    println(s"  - Available contracts: " + avail)

    players.keySet.toSeq.sortBy { s => s } foreach { name =>
      println(s"\n## Player ${name.toUpperCase()}")
      val dataset: Seq[Result] = results.toSeq filter { _.name == name } sortBy { _.islandName }
      val retrieved = (dataset filter { d => d.isInstanceOf[OK]} flatMap {
        r => extractContracts(r.asInstanceOf[OK], jobs).toSeq
      }).size

      println(f"  - Collected contracts: $retrieved (${retrieved.toDouble / avail * 100.0}%.2f%%)")

      println("  - Detailed results:")
      dataset foreach { r =>
        print(s"    - Using island ${r.islandName}, ")
        r match {
          case KO(_,_,reason,_,_) => {  println("KO - " + reason) }
          case OK(_,_,remaining, resources,_,_,_) => {
            val contracts = extractContracts(r.asInstanceOf[OK],jobs)
            print("completed : " + (if (contracts.isEmpty) "none" else contracts.mkString(",")))
            println(s" / ${remaining} action points left")
            resources foreach { case (r,i) => println(s"      - $r: $i") }
          }
        }
      }
    }
  }


  private def displayJobs(jobs: Set[Job]): Unit = {
    println("## Jobs involved in the retrospective")
    val sorted = jobs.toSeq sortBy { _.islandData.name }
    sorted foreach { j =>
       println("  - " + j.toString)
    }
  }

  private def extractContracts(res: OK, jobs: Set[Job]): Set[Resource] = {
    val theJob = (jobs.find { _.islandData.name == res.islandName }).get
    val theObjs = theJob.contract.objectives
    val collected = res.resources
    val obtained = theObjs filter { case (res, amount) =>
        collected.find { case(cRes, _) => cRes == res} match {
          case Some((r,i)) => i >= amount
          case None => false
        }
    }
    obtained map { case (res, amount) => res }
  }

  protected def export2json(fileName: String, jobs: Set[Job], results: Set[Result]) {
    println("## Exporting to JSON")

    val jobsData = new JSONArray()
    jobs map { j => jobsData.put(j.toJson) }

    val resultData = new JSONArray()
    results map { r => resultData.put(r.toJson) }

    val json = new JSONObject()
        .put("jobs",    jobsData)
        .put("results", resultData)

    val writer = new PrintWriter(new File(fileName))
    try { json.write(writer) } finally { writer.close() }
    println(s"  - JSON dataset available in $fileName")
  }

}
