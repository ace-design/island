package eu.ace_design.island.arena

import java.io.{File, PrintWriter}

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
    val runner = Runner(outputDir = outputDir)
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

    println("  - Execution time: " + delta + "(ms)\n")

    players foreach { case (name, _) =>
      println(s"## $name: ")
      val dataset = results.toSeq filter { _.name == name } sortBy { _.islandName }
      dataset foreach { r =>
        print(s"  - ${r.islandName}: ")
        r match {
          case KO(_,_,reason,_) => {  println("KO - " + reason) }
          case OK(_,_,remaining, resources, _) => {
            println(s"${remaining} left - completed: " + extractContracts(r.asInstanceOf[OK],jobs))
            resources foreach { case (r,i) => println(s"    - $r: $i") }
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
    println("# Exporting to JSON")

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
