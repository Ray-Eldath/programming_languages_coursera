module Doubler
  def double
    self + self
  end
end

class String
  include Doubler  # implementing Mixin for already-defined class (Extension Method?)
end

class MyRange
  include Doubler, Enumerable#, Comparable
  attr_reader :first, :last
  def initialize(first, last)
    @first = first
    @last = last
  end

  def each  # implementing Mixin for newly-defined class
    i = @first
    while i < @last
      yield i
      i += 1
    end
  end

  def + x  # implementing Mixin for newly-defined class
    f = first
    f = x.first if x.first < first
    l = last
    l = x.last if x.last > last
    MyRange.new(f, l)
  end
end

# seems Typeclass is somewhat equal to Mixin.
