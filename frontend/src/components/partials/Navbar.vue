<template>
  <header class="bg-gray-900 sm:flex sm:justify-between sm:px-4 sm:py-3 sm:items-center">
    <div class="flex items-center justify-between px-4 py-3 sm:p-0">
      <div class="flex items-center">
        <img class="h-8" src="../../assets/logo_white.png" alt="Timesheet"/>
        <a class="ml-2 text-2xl font-medium text-white" href="/">Timesheet</a>
      </div>
      <div class="sm:hidden">
        <button @click="isOpen = !isOpen" type="button" class="block text-gray-500 hover:text-white focus:text-white focus:outline-none">
          <svg class="h-6 w-6 fill-current" viewBox="0 0 24 24">
            <path v-if="isOpen" fill-rule="evenodd" d="M18.278 16.864a1 1 0 0 1-1.414 1.414l-4.829-4.828-4.828 4.828a1 1 0 0 1-1.414-1.414l4.828-4.829-4.828-4.828a1 1 0 0 1 1.414-1.414l4.829 4.828 4.828-4.828a1 1 0 1 1 1.414 1.414l-4.828 4.829 4.828 4.828z"/>
            <path v-if="!isOpen" fill-rule="evenodd" d="M4 5h16a1 1 0 0 1 0 2H4a1 1 0 1 1 0-2zm0 6h16a1 1 0 0 1 0 2H4a1 1 0 0 1 0-2zm0 6h16a1 1 0 0 1 0 2H4a1 1 0 0 1 0-2z"/>
          </svg>
        </button>
      </div>
    </div>
    <nav :class="isOpen ? 'block' : 'hidden'" class="sm:block">
      <div class="px-2 pt-2 pb-4 sm:flex sm:items-center sm:p-0">
        <router-link to="/" class="block px-2 py-1 text-white font-semibold rounded hover:bg-gray-800 sm:text-sm sm:mt-0 sm:ml-2">Home</router-link>
        <router-link to="/about" class="mt-1 block px-2 py-1 text-white font-semibold rounded hover:bg-gray-800 sm:text-sm sm:mt-0 sm:ml-2">About</router-link>
        <AccountDropdown v-if="$auth.isAuthenticated" class="hidden sm:block sm:ml-6" v-on:login="login" v-on:logout="logout"/>
        <a class="mt-1 block px-2 py-1 text-white rounded hover:bg-gray-800 sm:text-sm sm:mt-0 sm:ml-2" href="#" v-if="!$auth.isAuthenticated && !$auth.loading" @click="login">Sign in</a>
      </div>
      <div class="px-4 py-5 border-t border-gray-800 sm:hidden">
        <div v-if="$auth.isAuthenticated" class="flex items-center">
          <img class="h-8 w-8 border-2 border-gray-600 rounded-full object-cover" alt="Profile picture" :src="$auth.user.picture">
          <span class="ml-3 font-semibold text-white">{{ $auth.user.name }}</span>
        </div>
        <div :class="{'mt-4': $auth.isAuthenticated}">
          <div :class="{'mt-2': $auth.isAuthenticated}" class="block text-gray-400 hover:text-white" v-if="!$auth.loading">
            <a class="block" href="#" v-if="!$auth.isAuthenticated" @click="login">Sign in</a>
            <a class="block" href="#" v-if="$auth.isAuthenticated" @click="logout">Log out</a>
          </div>
        </div>
      </div>
    </nav>
  </header>
</template>

<script>
import AccountDropdown from '@/components/partials/AccountDropdown.vue';

export default {
  name: 'Nav',
  components: {
    AccountDropdown,
  },
  data() {
    return {
      isOpen: false,
    };
  },
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
