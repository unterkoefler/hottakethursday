class TakeController < ApplicationController
  protect_from_forgery with: :null_session
  respond_to :json
  before_action :authenticate_user!

  include TakeHelper

  before_action :confirm_thursday!
  def confirm_thursday!
    unless thursday?(Time.now) || Rails.env == 'development'
      render json: 'ITS NOT THURSDAY'
    end
  end

  def all
    # TODO: perf. probably need to add an index also
    render json: Take.all.sort_by(&:created_at)
  end

  def all_from_today
    now = Time.now
    Take.where(created_at: (now - 27.hours)..(now + 3.hours)) # bit of leeway
  end

  def create
    params.require(:contents)
    current_user.make_the_hottest_of_takes!(params[:contents])
  end

  def like
    params.require(:take_id)
    take = Take.find_by(id: params[:take_id])
    current_user.like!(take)
  end

  def unlike
    params.require(:take_id)
    take = Take.find_by(id: params[:take_id])
    current_user.unlike!(take)
  end
end
