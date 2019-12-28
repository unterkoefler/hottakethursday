class MeController < ApplicationController
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


end
