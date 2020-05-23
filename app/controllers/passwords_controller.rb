class PasswordsController < Devise::PasswordsController
  protect_from_forgery with: :null_session
  respond_to :json

  def create
    self.resource = resource_class.send_reset_password_instructions(params)

    if successfully_sent?(resource)
      render json: "Email sent"
    else
      raise "Failed to send email"
    end
  end

  def edit
    token = params[:reset_password_token]
    path = "/reset-password?reset_password_token=#{token}"
    redirect_to path
  end

  def update
    self.resource = resource_class.reset_password_by_token(params)
  end
end
