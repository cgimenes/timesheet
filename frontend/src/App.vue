<template>
  <div id="app" class="antialiased text-gray-900">
    <div class="bg-gray-100 min-h-screen xl:flex xl:flex-col xl:h-screen">
      <Navbar />
      <div class="xl:flex-1 xl:flex xl:overflow-y-hidden">
        <Sidebar />
        <router-view class="py-6 xl:flex-1 xl:overflow-x-hidden"/>
      </div>
    </div>
  </div>
</template>

<script>
import Navbar from '@/components/partials/Navbar.vue';
import Sidebar from '@/components/partials/Sidebar.vue';

export default {
  name: 'App',
  components: {
    Navbar,
    Sidebar,
  },
};
</script>

<style lang="scss">

</style>
