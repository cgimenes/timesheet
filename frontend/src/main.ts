import Vue from 'vue';
import App from './App.vue';
import './registerServiceWorker';
import router from './router';
import { Auth0Plugin } from './auth';
import './assets/styles/index.scss';

Vue.use(Auth0Plugin, {
  domain: 'dev-03ovpnrf.us.auth0.com',
  clientId: 'n4jSE2RqFNwE2JTgya1bS6ZBeoTScmNB',
  onRedirectCallback: (appState: any) => {
    router.push(
      appState && appState.targetUrl
        ? appState.targetUrl
        : window.location.pathname,
    );
  },
});

Vue.config.productionTip = false;

new Vue({
  router,
  render: (h) => h(App),
}).$mount('#app');
