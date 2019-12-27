class TakeController < ApplicationController
  def all
    # TODO: perf. probably need to add an index.
    Take.all.sort_by(&:created_at)
  end
end
