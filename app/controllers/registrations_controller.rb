class RegistrationsController < Devise::RegistrationsController
  protect_from_forgery with: :null_session
  respond_to :json

  def create
    #user = User.create(email: params['email'], password: params['password'])
    #user.save
    #render_resource user

    # This code _should_ work, but it doesn't
    build_resource(sign_up_params)
    resource.save
    render_resource(resource)
  end

  private

  def render_resource(resource)
    if resource.errors.empty?
      render json: resource
    else
      validation_error(resource)
    end
  end

  def validation_error(resource)
    render json: {
        errors: [
            {
                status: '400',
                title: 'Bad Request',
                detail: resource.errors,
                code: '100'
            }
        ]
    }, status: :bad_request
  end
end