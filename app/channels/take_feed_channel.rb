class TakeFeedChannel < ApplicationCable::Channel
  def subscribed
    stream_from "take_feed_channel"
  end

  def unsubscribed
    # Any cleanup needed when channel is unsubscribed
  end
end
