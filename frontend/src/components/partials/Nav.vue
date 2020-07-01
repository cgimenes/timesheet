<template>
  <nav>
    <router-link to="/" class="navbar-item">Home</router-link>
    <router-link to="/about" class="navbar-item">About</router-link>
    <div v-if="!$auth.loading">
      <a href="#" v-if="!$auth.isAuthenticated" @click="login">Sign in</a>
      <a href="#" v-if="$auth.isAuthenticated" @click="logout">Log out</a>
    </div>
  </nav>
</template>
<script>
export default {
  name: 'Nav',
  methods: {
    login() {
      this.$auth.loginWithRedirect();
    },
    logout() {
      this.$auth.logout({
        returnTo: window.location.origin,
      });
    },
  },
};
</script>
<style lang="scss" scoped>

</style>
