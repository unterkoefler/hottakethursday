class TakeController < ApplicationController
  protect_from_forgery with: :null_session
  respond_to :json
  before_action :authenticate_user!

  def all
    # TODO: perf. probably need to add an index also
    render json: Take.all.sort_by(&:created_at)
  end

  def create
    params.require(:contents)
    current_user.make_the_hottest_of_takes!(params[:contents])
  end

  def like
    params.require(:post_id)
    take = Take.find_by(id: params[:post_id])
    current_user.like!(take)
  end

  def unlike
    params.require(:post_id)
    take = Take.find_by(id: params[:post_id])
    current_user.unlike!(take)
  end
end
