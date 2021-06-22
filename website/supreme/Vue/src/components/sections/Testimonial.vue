<template>
    <section
        class="testimonial section center-content"
        :class="[
            topOuterDivider && 'has-top-divider',
            bottomOuterDivider && 'has-bottom-divider',
            hasBgColor && 'has-bg-color',
            invertColor && 'invert-color'
        ]">
        <div class="container">
            <div
                class="testimonial-inner section-inner"
                :class="[
                    topDivider && 'has-top-divider',
                    bottomDivider && 'has-bottom-divider'
                ]">
                <c-section-header :data="sectionHeader" class="center-content" />
                <div
                    class="tiles-wrap illustration-section-04 reveal-from-top"
                    :class="[
                        pushLeft && 'push-left',
                    ]">
                    <div class="tiles-item reveal-from-top" data-reveal-delay="200">
                        <div class="tiles-item-inner has-shadow">
                            <div class="testimonial-item-header">
                                <div class="testimonial-item-image mt-8 mb-24">
                                    <c-image
                                        :src="require('@/assets/images/testimonial-image-01.png')"
                                        alt="Testimonial 01"
                                        :width="56"
                                        :height="56" />
                                </div>
                            </div>
                            <div class="testimonial-item-content">
                                <p class="text-sm mb-24">
                                    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
                                    Ut enim ad minim veniam, quis nostrud labore et dolore magna aliqua.
                                </p>
                            </div>
                            <div class="testimonial-item-footer has-top-divider">
                                <div class="testimonial-item-name h6 m-0">
                                    Erika Muliawan
                                </div>
                                <div class="testimonial-item-link text-xs">
                                    <a href="#">AppName</a>
                                </div>
                            </div>
                        </div>
                    </div>

                    <div class="tiles-item reveal-from-top" data-reveal-delay="300">
                        <div class="tiles-item-inner has-shadow">
                            <div class="testimonial-item-header">
                                <div class="testimonial-item-image mt-8 mb-24">
                                    <c-image
                                        :src="require('@/assets/images/testimonial-image-02.png')"
                                        alt="Testimonial 02"
                                        :width="56"
                                        :height="56" />
                                </div>
                            </div>
                            <div class="testimonial-item-content">
                                <p class="text-sm mb-24">
                                    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
                                    Ut enim ad minim veniam, quis nostrud labore et dolore magna aliqua.
                                </p>
                            </div>
                            <div class="testimonial-item-footer has-top-divider">
                                <div class="testimonial-item-name h6 m-0">
                                    Erika Muliawan
                                </div>
                                <div class="testimonial-item-link text-xs">
                                    <a href="#">AppName</a>
                                </div>
                            </div>
                        </div>
                    </div>

                    <div class="tiles-item reveal-from-top">
                        <div class="tiles-item-inner has-shadow">
                            <div class="testimonial-item-header">
                                <div class="testimonial-item-image mt-8 mb-24">
                                    <c-image
                                        :src="require('@/assets/images/testimonial-image-03.png')"
                                        alt="Testimonial 03"
                                        :width="56"
                                        :height="56" />
                                </div>
                            </div>
                            <div class="testimonial-item-content">
                                <p class="text-sm mb-24">
                                    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
                                    Ut enim ad minim veniam, quis nostrud labore et dolore magna aliqua.
                                </p>
                            </div>
                            <div class="testimonial-item-footer has-top-divider">
                                <div class="testimonial-item-name h6 m-0">
                                    Erika Muliawan
                                </div>
                                <div class="testimonial-item-link text-xs">
                                    <a href="#">AppName</a>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </section>
</template>

<script>
import { SectionTilesProps } from '@/utils/SectionProps.js'
import CSectionHeader from '@/components/sections/partials/SectionHeader.vue'
import CImage from '@/components/elements/Image.vue'

export default {
  name: 'CTestimonial',
  components: {
    CSectionHeader,
    CImage
  },
  mixins: [SectionTilesProps],
  data() {
    return {
      sectionHeader: {
        title: 'Customer Testimonials',
        paragraph: ''
      }
    }
  }
}
</script>