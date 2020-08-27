import Vue from 'vue';
import Vuex from 'vuex';

Vue.use(Vuex);

const store = new Vuex.Store({
  state: {
    showMoreInfo: false,
    actualMonth: 0,
    timesheet: null as unknown as {months: object[]},
    view: 'cards',
  },
  mutations: {
    toggleShowMoreInfo(state) {
      state.showMoreInfo = !state.showMoreInfo;
    },
    setTimesheet(state, timesheet) {
      state.timesheet = timesheet;
      state.actualMonth = timesheet.months.length - 1;
    },
    nextMonth(state) {
      if (state.actualMonth < state.timesheet.months.length - 1) {
        state.actualMonth += 1;
      }
    },
    previousMonth(state) {
      if (state.actualMonth > 0) {
        state.actualMonth -= 1;
      }
    },
  },
});

export default store;
