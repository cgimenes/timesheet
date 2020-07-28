<template>
  <main>
    <button @click="load">Load</button>
    <label class="block w-full px-2 sm:w-1/2 lg:w-full" v-if="actualMonth !== null">
      <span class="text-sm font-semibold text-gray-500">Month</span>
      <div class="flex justify-between">
        <span class="text-sm text-gray-500">{{timesheet.months[actualMonth].month}}</span>
        <div class="flex justify-between w-16 px-1 text-gray-500">
          <div class="h-6 w-6 flex justify-center items-center hover:bg-gray-900 rounded-full cursor-pointer" @click="previousMonth">
            <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="w-4 h-4">
              <polyline points="15 18 9 12 15 6"></polyline>
            </svg>
          </div>
          <div class="h-6 w-6 flex justify-center items-center hover:bg-gray-900 rounded-full cursor-pointer" @click="nextMonth">
            <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="w-4 h-4">
              <polyline points="9 18 15 12 9 6"></polyline>
            </svg>
          </div>
        </div>
      </div>
    </label>
    <Timesheet v-bind:month="timesheet.months[actualMonth]" v-if="timesheet" v-bind:view="view"/>
  </main>
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
      view: 'cards',
      actualMonth: null,
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
          this.actualMonth = 0;
        })
        .catch((error) => {
          console.log(error);
        });
    },
    nextMonth() {
      this.actualMonth += 1;
    },
    previousMonth() {
      this.actualMonth -= 1;
    },
  },
};
</script>

<style lang="scss" scoped>
</style>
