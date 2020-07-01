<template>
  <div id="app">
    <Nav />
    <router-view/>
  </div>
</template>

<script>
// @ is an alias to /src
import Nav from '@/components/partials/Nav.vue';

export default {
  name: 'App',
  components: {
    Nav,
  },
};
</script>

<style lang="scss">

</style>
