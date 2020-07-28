<template>
  <section class="bg-gray-800 sm:w-64">
    <div class="flex justify-between px-4 py-3 xl:hidden">
      <div class="relative max-w-xs w-full">
        <div class="absolute inset-y-0 left-0 flex items-center pl-3">
          <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg" class="h-6 w-6 fill-current text-gray-600">
            <path d="M16.32 14.9l1.1 1.1c.4-.02.83.13 1.14.44l3 3a1.5 1.5 0 0 1-2.12 2.12l-3-3a1.5 1.5 0 0 1-.44-1.14l-1.1-1.1a8 8 0 1 1 1.41-1.41l.01-.01zM10 16a6 6 0 1 0 0-12 6 6 0 0 0 0 12z"></path>
          </svg>
        </div>
        <input placeholder="Search by keywords" class="block w-full bg-gray-900 focus:outline-none focus:bg-white focus:text-gray-900 text-white rounded-lg pl-10 pr-4 py-2">
      </div>
      <button type="button" class="ml-4 inline-flex items-center hover:bg-gray-600 focus:outline-none focus:shadow-outline rounded-lg shadow pl-3 pr-4 bg-gray-700">
        <svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" class="h-6 w-6 fill-current text-gray-500">
          <path d="M3 6a1 1 0 0 1 1-1h16a1 1 0 1 1 0 2H4a1 1 0 0 1-1-1zm3 6a1 1 0 0 1 1-1h10a1 1 0 1 1 0 2H7a1 1 0 0 1-1-1zm4 5a1 1 0 1 0 0 2h4a1 1 0 1 0 0-2h-4z"></path>
        </svg>
        <span class="ml-1 text-white font-medium">Filters</span>
      </button>
    </div>
    <form class="xl:h-full xl:flex xl:flex-col xl:justify-between hidden">
      <div class="lg:flex xl:block xl:overflow-y-auto">
        <div class="px-4 py-4 border-t border-gray-900 lg:w-1/3 xl:border-t-0 xl:w-full">
          <div class="flex flex-wrap -mx-2">
            <div class="relative w-1/2 px-2 sm:w-1/4 lg:w-3/5">
              <div class="absolute inset-y-0 left-0 flex items-center pl-3">
                <svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg" class="h-6 w-6 stroke-current text-gray-600">
                  <path d="M8 7V3M16 7V3M7 11H17M5 21H19C20.1046 21 21 20.1046 21 19V7C21 5.89543 20.1046 5 19 5H5C3.89543 5 3 5.89543 3 7V19C3 20.1046 3.89543 21 5 21Z" stroke="#4A5568" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
              </div>
              <input v-model="date" placeholder="Date" class="block w-full bg-gray-900 shadow focus:outline-none focus:bg-white focus:text-gray-900 text-white rounded-lg pl-8 pr-1 py-2">
            </div>
            <div class="relative w-1/2 px-2 sm:w-1/4 lg:w-2/5">
              <div class="absolute inset-y-0 left-0 flex items-center pl-3">
                <svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg" class="h-6 w-6 stroke-current text-gray-600">
                  <path d="M12 8V12L15 15M21 12C21 16.9706 16.9706 21 12 21C7.02944 21 3 16.9706 3 12C3 7.02944 7.02944 3 12 3C16.9706 3 21 7.02944 21 12Z" stroke="#4A5568" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
              </div>
              <input v-model="time" placeholder="Time" class="block w-full bg-gray-900 shadow focus:outline-none focus:bg-white focus:text-gray-900 text-white rounded-lg pl-8 pr-1 py-2">
            </div>
            <button @click="addPunch" class="mt-4 mx-2 block shadow w-full sm:w-auto sm:inline-block bg-indigo-500 hover:bg-indigo-400 font-semibold text-white px-4 py-2 rounded-lg xl:block xl:w-full">Add punch</button>
          </div>
        </div>
      </div>
    </form>
  </section>
</template>

<script>

export default {
  name: 'Nav',
  components: {
  },
  data() {
    return {
      date: '2020-01-01',
      time: '11:00',
    };
  },
  methods: {
    addPunch(e) {
      e.preventDefault();
      console.log(this.date);
      console.log(this.time);
    },
  },
};
</script>

<style lang="scss" scoped>
</style>
