class MeController < ApplicationController
  # Echoes either user information or nil
  def me
    render json: current_user
  end

  # https://cameronbothner.com/activestorage-beyond-rails-views/
  def upload_avatar
    params.require(:user).require(:avatar) do |params|
      avatar = params[:user][:avatar]
      current_user.avatar.attach(avatar)
    end
  end
end
