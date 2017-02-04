package jp.cedretaber.yhpg.e11

case class Node(n: Int, children: Seq[Node])
case class Cursor(node: Node, parent: Option[Cursor])

object Main {

  def listDivisors(n: Int) = (1 to (n/2)).filter { i => n % i == 0 }

  def mkTree(n: Int): Node = Node(n, listDivisors(n).collect { case i if i + 1 > 2 && i + 1 < n => mkTree(i+1) })

  def search(n: Int, m: Int, cur: Cursor): Int = {
    def impl(scur: Cursor, step: Int, exclude: Node): Int =
      if (scur.node.n == m)
        step
      else
        (scur.node.children.collect {
          case child if child != exclude => impl(Cursor(child, Some(scur)), step + 1, scur.node)
        } :+ (
          scur.parent match {
            case None => Int.MaxValue
            case Some(par) if par.node == exclude => Int.MaxValue
            case Some(par) => impl(par, step + 1, scur.node)
          }
        ) :+ Int.MaxValue).min

    if (cur.node.n == n)
      impl(cur, 0, cur.node)
    else
      (cur.node.children.map { child => search(n, m, Cursor(child, Some(cur))) } :+ Int.MaxValue).min
  }


  def test(input: String, expected: String) = {
    val Array(root, n, m) = input.split("[:,]").map(_.toInt)

    val tree = mkTree(root)
    val ret = search(n, m, Cursor(tree, None))

    println {
      if (ret == expected.toInt)
        s"pass. ret = $ret"
      else
        s"failed. expected $expected, but $ret"
    }
  }


  def main(arg: Array[String]): Unit = {
    /*0*/ test( "50:6,3", "1" );
    /*1*/ test( "98:5,11", "4" );
    /*2*/ test( "1000:33,20", "7" );
    /*3*/ test( "514:9,18", "8" );
    /*4*/ test( "961:5,4", "3" );
    /*5*/ test( "1369:1369,3", "2" );
    /*6*/ test( "258:16,12", "5" );
    /*7*/ test( "235:13,3", "2" );
    /*8*/ test( "1096:19,17", "8" );
    /*9*/ test( "847:7,17", "6" );
    /*10*/ test( "1932:3,5", "2" );
    /*11*/ test( "2491:4,8", "3" );
    /*12*/ test( "840:421,36", "2" );
    /*13*/ test( "1430:37,111", "3" );
    /*14*/ test( "496:17,9", "2" );
    /*15*/ test( "891:6,10", "1" );
    /*16*/ test( "1560:196,21", "2" );
    /*17*/ test( "516:20,12", "5" );
    /*18*/ test( "696:30,59", "2" );
    /*19*/ test( "1760:5,441", "2" );
    /*20*/ test( "1736:11,26", "5" );
    /*21*/ test( "1518:17,34", "4" );
    /*22*/ test( "806:63,16", "5" );
    /*23*/ test( "1920:3,97", "2" );
    /*24*/ test( "1150:13,22", "4" );
    /*25*/ test( "920:116,5", "1" );
    /*26*/ test( "2016:7,337", "2" );
    /*27*/ test( "408:9,25", "2" );
    /*28*/ test( "735:36,8", "2" );
    /*29*/ test( "470:5,31", "2" );
    /*30*/ test( "2100:12,351", "3" );
    /*31*/ test( "870:36,10", "1" );
    /*32*/ test( "1512:253,13", "2" );
    /*33*/ test( "697:12,15", "3" );
    /*34*/ test( "1224:5,14", "2" );
    /*35*/ test( "986:125,17", "3" );
    /*36*/ test( "864:12,13", "3" );
    /*37*/ test( "500:21,51", "2" );
    /*38*/ test( "819:33,21", "4" );
    /*39*/ test( "594:55,3", "2" );
    /*40*/ test( "638:17,24", "3" );
  }
}
