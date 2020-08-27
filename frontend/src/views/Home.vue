<template>
  <main>
    <Timesheet v-bind:month="$store.state.timesheet.months[actualMonth]" v-if="timesheet" v-bind:view="view"/>
  </main>
</template>

<script>
import Timesheet from '@/components/partials/Timesheet.vue';

export default {
  name: 'Home',
  components: {
    Timesheet,
  },
};
</script>

<style lang="scss" scoped>
</style>
