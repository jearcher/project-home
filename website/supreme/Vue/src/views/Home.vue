<template>
    <fragment>
        <c-hero-full has-bg-color invert-color class="illustration-section-01" />
        <c-features-tiles />
        <c-features-split top-divider invert-mobile image-fill />
        <c-testimonial top-divider />
        <c-cta has-bg-color invert-color split />
    </fragment>
</template>

<script>
// import layout
import CLayout from '@/layouts/LayoutDefault.vue'
// import sections
import CHeroFull from '@/components/sections/HeroFull.vue'
import CFeaturesTiles from '@/components/sections/FeaturesTiles.vue'
import CFeaturesSplit from '@/components/sections/FeaturesSplit.vue'
import CTestimonial from '@/components/sections/Testimonial.vue'
import CCta from '@/components/sections/Cta.vue'

export default {
  name: 'Home',
  components: {
    CHeroFull,
    CFeaturesTiles,
    CFeaturesSplit,
    CTestimonial,
    CCta
  },
  created() {
    this.$emit('update:layout', CLayout)
  }
}
</script>
