class Exp
end

class Int < Exp
  attr_reader :i
  def initialize i
    @i = i
  end

  def eval
    self
  end

  def add x  # first dispath
    x.add_int self  # double dispath
  end

  def add_int x
    Int.new(x.i + i)
  end

  def add_string x
    MyString.new(x.s + i.to_s)
  end
end

class MyString < Exp
  attr_reader :s
  def initialize s
    @s = s
  end

  def eval
    self
  end

  def add x  # first dispath
    x.add_string self  # double dispath
  end

  def add_int x
    MyString.new(x.i.to_s + s)
  end

  def add_string x
    MyString.new(x.s + s)
  end
end
