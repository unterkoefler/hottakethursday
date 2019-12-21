Rails.application.routes.draw do
  # For details on the DSL available within this file, see https://guides.rubyonrails.org/routing.html
  namespace :api do
    namespace :v1 do
      devise_for :users,
                 path: '',
                 path_names: {
                     sign_in: 'login',
                     sign_out: 'logout',
                     registration: 'signup'
                 },
                 controllers: { sessions: 'sessions',
                                registrations: 'registrations' },
                 defaults: { format: :json }
    end
  end

  root 'root#spa'
  get '*route', to: 'root#spa'
end
