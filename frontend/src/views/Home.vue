<template>
  <div>
    <!-- <h3 v-if="$auth.isAuthenticated">Welcome, {{ $auth.user.name }} ({{ $auth.user.email }})!</h3> -->
    <button @click="load">Load</button>
    <Timesheet v-bind:month="timesheet.months[1]" v-if="timesheet"/>
  </div>
</template>

<script>
import axios from 'axios';
import Timesheet from '@/components/partials/Timesheet.vue';

export default {
  name: 'Home',
  components: {
    Timesheet,
  },
  data() {
    return {
      timesheet: null,
    };
  },
  methods: {
    async load() {
      axios.defaults.baseURL = 'http://localhost:8082/api/timesheet';
      const token = await this.$auth.getTokenSilently();
      axios.defaults.headers.common.Authorization = `Token ${token}`;

      axios.get('/')
        .then((response) => {
          this.timesheet = response.data.timesheet;
        })
        .catch((error) => {
          console.log(error);
        });
    },
  },
};
</script>

<style lang="scss" scoped>
</style>
