<template>
  <div class="relative">
    <button @click="isOpen = !isOpen" class="relative z-10 block h-8 w-8 rounded-full overflow-hidden border-2 border-gray-600 focus:outline-none focus:border-white hover:border-white">
      <img class="h-full w-full object-cover" alt="Profile picture" :src="$auth.user.picture">
    </button>
    <button v-if="isOpen" @click="isOpen = false" tabindex="-1" class="fixed inset-0 h-full w-full cursor-default"></button>
    <div v-if="isOpen" class="absolute right-0 mt-2 py-2 w-48 bg-white rounded-lg shadow-xl">
      <div class="px-4 py-2 text-gray-800 hover:text-white hover:bg-indigo-500" v-if="!$auth.loading">
        <a class="block" href="#" v-if="!$auth.isAuthenticated" @click="$emit('login')">Sign in</a>
        <a class="block" href="#" v-if="$auth.isAuthenticated" @click="$emit('logout')">Log out</a>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'AccountDropdown',
  data() {
    return {
      isOpen: false,
    };
  },
  methods: {
  },
  created() {
    const handleEscape = (e) => {
      if (e.key === 'Esc' || e.key === 'Escape') {
        this.isOpen = false;
      }
    };

    document.addEventListener('keydown', handleEscape);

    this.$once('hook:beforeDestroy', () => {
      document.removeEventListener('keydown', handleEscape);
    });
  },
};
</script>

<style lang="scss" scoped>

</style>
