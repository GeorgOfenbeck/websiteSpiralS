package SpiralS2

/**
  * Created by rayda on 11-Jan-17.
  */
object ExportGraph extends App {


  def createMixNode(label: String, name: String): String = {
    val x =
      s"""node$label [label=<
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
      <TR>
        <TD colspan="2"> $name </TD>
      </TR>
      <TR>
        <TD>Dynamic</TD>
        <TD PORT="f1">Int.MIN.. Int.MAX</TD>
      </TR>
    </TABLE>>];
    """
    x
  }


  def createMixCluster(name: String): String = {
    val t = (0 until 10).foldLeft("") ( (acc,ele) => acc ++ createMixNode(ele.toString,s"name$ele"))
    val ie = (0 until 9).foldLeft("") ( (acc,ele) => acc ++  s"node$ele -> node${ele+1} [style=invis]\n")
    mixcluster(t ++ ie,name)

  }

  def functioncluster(content: String, name: String): String = {
    s"""subgraph cluster_$name {
      label   = $name;
      color   = "gray";
      $content
    }
    """
  }


  def mixcluster(content: String, label: String): String = {
    s"""subgraph cluster_$label {
      label   = "Parameter Mix";
      color   = "gray";
      $content
    }
    """
  }


  def graphheader(): String = {
    s"""digraph callgraph {
      node [shape=plaintext]
      """
  }


  def export() = {
    val file = new java.io.FileOutputStream("F:\\PhD\\git\\code\\DualDev\\virtualization-lms-core\\Graph.dot")
    val stream2 = new java.io.PrintWriter(file)
    //dumpCode (stream2)
    stream2.println(graphheader())
    stream2.println(functioncluster(createMixCluster("test"), "DFT"))
    stream2.println()
    stream2.println("\n}")
    stream2.flush()
    stream2.close()
    file.close()
  }

  export()

}
