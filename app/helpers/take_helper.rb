module TakeHelper
  def self.thursday?
    now = Time.now
    (now.in_time_zone('Eastern Time (US & Canada)').thursday? ||
        now.in_time_zone('Pacific Time (US & Canada)').thursday?)
  end

end
