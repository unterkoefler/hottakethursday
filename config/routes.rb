Rails.application.routes.draw do
  # For details on the DSL available within this file, see https://guides.rubyonrails.org/routing.html
  scope :api do
    scope :v1 do
      devise_for :users,
                 controllers: { sessions: 'sessions',
                                registrations: 'registrations' },
                 defaults: { format: :json }
    end
  end

  root 'root#spa'
  get '*route', to: 'root#spa'
end
