<template>
  <table>
      <thead>
          <tr>
              <th>Day</th>
              <th>Punches</th>
              <th>Worked</th>
              <th>Left</th>
              <th>Balance</th>
          </tr>
      </thead>
      <tbody>
        <tr v-for="day in month.days" :key="day.date">
            <td>{{day.date}}</td>
            <td v-for="punch in day.punches" :key="punch">
              <span>
                {{punch}} <a :href="`/timesheet/remove?datetime=${day.date}T${punch}`">X</a>
              </span>
            </td>
            <td>{{day.worked}}</td>
            <td>{{day.left}}</td>
            <td>{{day.balance}}</td>
        </tr>
      </tbody>
      <tfoot>
          <tr>
              <td colspan="5"><strong>Month balance: {{month.balance}}</strong></td>
          </tr>
      </tfoot>
  </table>
</template>

<script>
export default {
  name: 'Timesheet',
  props: ['month'],
  data() {
    return {
    };
  },
  methods: {
  },
};
</script>

<style lang="scss" scoped>

</style>
