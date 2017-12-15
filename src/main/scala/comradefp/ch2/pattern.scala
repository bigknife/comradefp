package ch2.pattern

case class Member(name: String, amount: Int, age: Int)

object PerfectMember {
  def unapply(member: Member): Option[(String, Int, Int)] =
    if (member.amount > 1000 && member.age < 40) Some((member.name, member.amount, member.age))
    else None
}

object Matching {
  def demo(member: Member): Unit = member match {
    case PerfectMember(name, _, _) ⇒ println(s"$name is perfect")
    case Member(name, _, _)        ⇒ println(s"$name is not perfect")
  }
}
