<template>
    <section
        class="features-split section"
        :class="[
            topOuterDivider && 'has-top-divider',
            bottomOuterDivider && 'has-bottom-divider',
            hasBgColor && 'has-bg-color',
            invertColor && 'invert-color'
        ]">
        <div class="container">
            <div
                class="features-split-inner section-inner"
                :class="[
                    topDivider && 'has-top-divider',
                    bottomDivider && 'has-bottom-divider'
                ]">
                <c-section-header :data="sectionHeader" class="center-content" />
                <div
                    class="split-wrap"
                    :class="[
                        invertMobile && 'invert-mobile',
                        invertDesktop && 'invert-desktop',
                        alignTop && 'align-top'
                    ]">
                    <div class="split-item">
                        <div class="split-item-content center-content-mobile">
                            <h3 class="mt-0 mb-16 reveal-from-left" data-reveal-delay="200">
                                Discover, master, start
                            </h3>
                            <p class="m-0 reveal-from-left" data-reveal-delay="400">
                                Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
                            </p>
                        </div>
                        <div
                            class="split-item-image illustration-element-02 reveal-from-top"
                            :class="imageFill && 'split-item-image-fill'">
                            <c-image
                                class="has-shadow"
                                :src="require('@/assets/images/features-split-image.svg')"
                                alt="Features split image 01"
                                :width="528"
                                :height="396" />
                            <div style="position:absolute;bottom:-12.4%;">
                                <c-image
                                    :src="require('@/assets/images/features-split-top-01.png')"
                                    alt="Features split top 01"
                                    :width="528"
                                    :height="396" />
                            </div>
                        </div>
                    </div>

                    <div class="split-item">
                        <div class="split-item-content center-content-mobile">
                            <h3 class="mt-0 mb-16 reveal-from-right" data-reveal-delay="200">
                                Discover, master, start
                            </h3>
                            <p class="m-0 reveal-from-right" data-reveal-delay="400">
                                Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
                            </p>
                        </div>
                        <div
                            class="split-item-image illustration-element-03 reveal-from-top"
                            :class="imageFill && 'split-item-image-fill'">
                            <c-image
                                class="has-shadow"
                                :src="require('@/assets/images/features-split-image.svg')"
                                alt="Features split image 02"
                                :width="528"
                                :height="396" />
                            <div style="position:absolute;bottom:-13.1%;">
                                <c-image
                                    :src="require('@/assets/images/features-split-top-02.png')"
                                    alt="Features split top 02"
                                    :width="528"
                                    :height="396" />
                            </div>
                        </div>
                    </div>

                    <div class="split-item">
                        <div class="split-item-content center-content-mobile">
                            <h3 class="mt-0 mb-16 reveal-from-left" data-reveal-delay="200">
                                Discover, master, start
                            </h3>
                            <p class="m-0 reveal-from-left" data-reveal-delay="400">
                                Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
                            </p>
                        </div>
                        <div
                            class="split-item-image illustration-element-04 reveal-from-top"
                            :class="imageFill && 'split-item-image-fill'">
                            <c-image
                                class="has-shadow"
                                :src="require('@/assets/images/features-split-image.svg')"
                                alt="Features split image 03"
                                :width="528"
                                :height="396" />
                            <div style="position:absolute;bottom:-16.1%;left:10.1%;right:-10.1%;">
                                <c-image
                                    :src="require('@/assets/images/features-split-top-03.png')"
                                    alt="Features split top 03"
                                    :width="528"
                                    :height="396" />
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </section>
</template>

<script>
import { SectionSplitProps } from '@/utils/SectionProps.js'
import CSectionHeader from '@/components/sections/partials/SectionHeader.vue'
import CImage from '@/components/elements/Image.vue'

export default {
  name: 'CFeaturesSplit',
  components: {
    CSectionHeader,
    CImage
  },
  mixins: [SectionSplitProps],
  data() {
    return {
      sectionHeader: {
        title: 'Meet Supreme',
        paragraph: 'Lorem ipsum is common placeholder text used to demonstrate the graphic elements of a document or visual presentation.'
      }
    }
  }
}
</script>