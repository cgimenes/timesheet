<template>
  <div>
    <table v-if="view == 'table'">
      <thead>
        <tr>
          <th>Date</th>
          <th>Punches</th>
          <th>Worked</th>
          <th>Left</th>
          <th>Balance</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="day in month.days" :key="day.date" :class="dayClass(day)">
          <td class="text-left">{{day.date}}</td>
          <td class="text-left">
            <span v-for="punch in day.punches" :key="punch">
              {{punch}} <a :href="`/timesheet/remove?datetime=${day.date}T${punch}`">X</a>
            </span>
          </td>
          <td class="text-right">{{day.worked | duration}}</td>
          <td class="text-right">{{day.left | duration}}</td>
          <td class="text-right" :class="balanceClass(day.balance)">{{day.balance | duration}}</td>
        </tr>
      </tbody>
      <tfoot>
        <tr>
          <td class="text-right" colspan="5">
            <strong>Month balance: <span :class="{}">{{month.balance | duration}}</span></strong>
          </td>
        </tr>
      </tfoot>
    </table>
    <div v-if="view == 'cards'" class="flex flex-wrap justify-center text-xs">
      <div class="m-1 w-56" v-for="n in blankDays()" v-bind:key="n"></div>
      <div v-for="day in month.days" v-bind:key="day.date" class="flex flex-col justify-between m-1 w-56 border border-gray-400 shadow-sm rounded-lg p-3">
        <div>
          <div class="text-center mb-3">
            <div class="text-gray-600 uppercase">
              {{day.date | dayOfWeek}}
            </div>
            <div :class="day.date === today ? 'text-white bg-indigo-600 rounded-lg' : 'text-gray-800'" class="uppercase font-semibold">
              {{day.date | dayOfMonth}}
            </div>
          </div>
          <div class="flex flex-wrap justify-center text-gray-800">
            <span class="m-1 px-1 border border-gray-400 rounded" v-for="punch in day.punches" v-bind:key="punch">{{punch}}</span>
            <span v-if="!day.punches.length">-</span>
          </div>
        </div>
        <div class="mt-3">
          <div class="text-gray-600 flex justify-between">
            <span>Worked: </span>
            <span>{{day.worked | duration}}</span>
          </div>
          <div class="text-gray-600 flex justify-between">
            <span>Left: </span>
            <span>{{day.left | duration}}</span>
          </div>
          <div :class="balanceClass(day.balance)" class="flex justify-between">
            <span>Balance: </span>
            <span>{{day.balance | duration}}</span>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import moment from 'moment';

function pad(n, width) {
  return n.length >= width ? n : new Array(width - n.length + 1).join('0') + n;
}

export default {
  name: 'Timesheet',
  props: ['month', 'view'],
  data() {
    return {
      today: moment().format('YYYY-MM-DD'),
    };
  },
  methods: {
    balanceClass(balance) {
      const duration = moment.duration(balance);
      const negative = duration.asMinutes() < 0;
      return {
        'text-green-900': !negative,
        'text-red-900': negative,
      };
    },
    dayClass(day) {
      return {
        'bg-yellow-200': day.review,
        'text-yellow-600': day.review,
        'bg-gray-200': day['not-calculated'],
        'text-gray-600': day['not-calculated'],
      };
    },
    blankDays() {
      return (new Date(this.month.month).getDay()) + 1;
    },
  },
  filters: {
    duration(value) {
      const duration = moment.duration(value);
      let negativeSign = '';
      if (duration.asMinutes() < 0) {
        duration.abs();
        negativeSign = '-';
      }
      return `${negativeSign}${pad(duration.hours().toString(), 2)}:${pad(duration.minutes().toString(), 2)}`;
    },
    dayOfWeek(value) {
      const date = moment(value);
      return date.format('ddd');
    },
    dayOfMonth(value) {
      const date = moment(value);
      return date.date();
    },
  },
};
</script>

<style lang="scss" scoped>

</style>
