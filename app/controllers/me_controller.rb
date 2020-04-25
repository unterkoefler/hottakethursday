class MeController < ApplicationController
  protect_from_forgery with: :null_session
  respond_to :json
  # Echoes either user information or nil
  def me
    render json: current_user
  end

  def by_id
    params.require(:id)
    render json: User.find_by(id: params[:id])
  end

  def by_ids
    params.require(:ids)
    render json: params[:ids].split(',').map { |id| User.find_by(id: id) }
  end

  def change_avatar
    authenticate_user!
    params.require(:avatar)
    current_user.avatar.attach(params[:avatar])
    render json: current_user
  end

  def change_name
    authenticate_user!
    params.require(:full_name)
    current_user.update(full_name: params[:full_name])
    render json: current_user
  end

  def change_bio
    authenticate_user!
    params.require(:bio)
    current_user.update(bio: params[:bio])
    render json: current_user
  end

  def change_least_fav_color
    authenticate_user!
    params.require(:least_fav_color)
    current_user.update(least_fav_color: params[:least_fav_color])
    render json: current_user
  end
end
