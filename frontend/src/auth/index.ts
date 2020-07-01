import Vue from 'vue';
import createAuth0Client from '@auth0/auth0-spa-js';

const DEFAULT_REDIRECT_CALLBACK = (appState: any) => window.history.replaceState(
  {},
  document.title,
  window.location.pathname,
);

let instance: any;

export const getInstance = () => instance;

export const useAuth0 = ({
  onRedirectCallback = DEFAULT_REDIRECT_CALLBACK,
  redirectUri = window.location.origin,
  ...options
}) => {
  if (instance) return instance;

  // The 'instance' is simply a Vue object
  instance = new Vue({
    data() {
      return {
        loading: true,
        isAuthenticated: false,
        user: {},
        auth0Client: null,
        popupOpen: false,
        error: null,
      };
    },
    methods: {
      /** Authenticates the user using a popup window */
      async loginWithPopup(o: any) {
        this.popupOpen = true;

        try {
          await (this.auth0Client as any).loginWithPopup(o);
        } catch (e) {
          // eslint-disable-next-line
          console.error(e);
        } finally {
          this.popupOpen = false;
        }

        this.user = await (this.auth0Client as any).getUser();
        this.isAuthenticated = true;
      },
      /** Handles the callback when logging in using a redirect */
      async handleRedirectCallback() {
        this.loading = true;
        try {
          await (this.auth0Client as any).handleRedirectCallback();
          this.user = await (this.auth0Client as any).getUser();
          this.isAuthenticated = true;
        } catch (e) {
          this.error = e;
        } finally {
          this.loading = false;
        }
      },
      /** Authenticates the user using the redirect method */
      loginWithRedirect(o: any) {
        return (this.auth0Client as any).loginWithRedirect(o);
      },
      /** Returns all the claims present in the ID token */
      getIdTokenClaims(o: any) {
        return (this.auth0Client as any).getIdTokenClaims(o);
      },
      /** Returns the access token. If the token is invalid or missing, a new one is retrieved */
      getTokenSilently(o: any) {
        return (this.auth0Client as any).getTokenSilently(o);
      },
      /** Gets the access token using a popup window */

      getTokenWithPopup(o: any) {
        return (this.auth0Client as any).getTokenWithPopup(o);
      },
      /** Logs the user out and removes their session on the authorization server */
      logout(o: any) {
        return (this.auth0Client as any).logout(o);
      },
    },
    /** Use this lifecycle method to instantiate the SDK client */
    async created() {
      // Create a new instance of the SDK client using members of the given options object
      (this.auth0Client as any) = await createAuth0Client({
        domain: options.domain,
        client_id: options.clientId,
        audience: options.audience,
        redirect_uri: redirectUri,
      });

      try {
        // If the user is returning to the app after authentication...
        if (
          window.location.search.includes('code=')
          && window.location.search.includes('state=')
        ) {
          // handle the redirect and retrieve tokens
          const { appState } = await (this.auth0Client as any).handleRedirectCallback();

          // Notify subscribers that the redirect callback has happened, passing the appState
          // (useful for retrieving any pre-authentication state)
          onRedirectCallback(appState);
        }
      } catch (e) {
        this.error = e;
      } finally {
        // Initialize our internal authentication state
        this.isAuthenticated = await (this.auth0Client as any).isAuthenticated();
        this.user = await (this.auth0Client as any).getUser();
        this.loading = false;
      }
    },
  });

  return instance;
};

// Create a simple Vue plugin to expose the wrapper object throughout the application
export const Auth0Plugin = {
  install(_Vue: any, options: any) {
    _Vue.prototype.$auth = useAuth0(options);
  },
};
