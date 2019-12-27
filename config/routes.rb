Rails.application.routes.draw do
  # For details on the DSL available within this file, see https://guides.rubyonrails.org/routing.html
  scope :api do
    scope :v1, defaults: { format: :json } do
      devise_for :users,
                 controllers: { sessions: 'sessions',
                                registrations: 'registrations' }
      scope :users do
        get 'me', to: 'me#me'
      end

      scope :takes do
        get 'all', to: 'take#all'
        post 'create', to: 'take#create'
      end
    end
  end

  root 'root#spa'
  get '*route', to: 'root#spa'
end
