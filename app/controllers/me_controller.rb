class MeController < ApplicationController
  # Echoes either user information or nil
  def me
    render json: current_user
  end
end
