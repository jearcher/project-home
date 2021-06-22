<template>
    <section
        class="pricing section"
        :class="[
            topOuterDivider && 'has-top-divider',
            bottomOuterDivider && 'has-bottom-divider',
            hasBgColor && 'has-bg-color',
            invertColor && 'invert-color'
        ]">
        <div class="container">
            <div
                class="pricing-inner section-inner"
                :class="[
                    topDivider && 'has-top-divider',
                    bottomDivider && 'has-bottom-divider'
                ]">
                <c-section-header :data="sectionHeader" class="center-content" />
                <div
                    v-if="pricingSwitcher"
                    class="pricing-switcher center-content"
                >
                    <c-switch
                        v-model="priceChangerValue"
                        true-value="1"
                        false-value="0"
                        right-label="Billed Annually">
                        Billed Monthly
                    </c-switch>
                </div>
                <div
                    v-if="pricingSlider"
                    class="pricing-slider center-content"
                >
                    <label class="form-slider">
                        <span class="mb-16">How many users do you have?</span>
                        <input
                            type="range"
                            ref="slider"
                            v-model="priceChangerValue"
                            @input="handlePricingSlide"
                            />
                        </label>
                    <div ref="sliderValue" class="pricing-slider-value">
                        {{ getPricingData(priceInput) }}
                    </div>
                </div>                
                <div
                    class="tiles-wrap illustration-section-03 reveal-from-top"
                    :class="[
                        pushLeft && 'push-left',
                    ]">
                    <div class="tiles-item reveal-from-top" data-reveal-delay="200">
                        <div class="tiles-item-inner has-shadow">
                            <div class="pricing-item-content">
                                <div class="pricing-item-header center-content mb-24">
                                    <div class="pricing-item-title text-xxs fw-500 pb-12">
                                        Essential
                                    </div>
                                    <div class="pricing-item-price pt-24 pb-24">
                                        <span class="pricing-item-price-currency text-color-high">{{ getPricingData(this.priceOutput.plan1, 0) }}</span>
                                        <span class="pricing-item-price-amount h1">{{ getPricingData(this.priceOutput.plan1, 1) }}</span>/m
                                    </div>
                                </div>
                                <div class="pricing-item-features mb-40">
                                    <div class="pricing-item-features-title fw-500 text-xs text-color-high mb-24">
                                        Top features
                                    </div>
                                    <ul class="pricing-item-features-list list-reset text-xs">
                                        <li class="is-checked">Excepteur sint occaecat velit</li>
                                        <li class="is-checked">Excepteur sint occaecat velit</li>
                                        <li class="is-checked">Excepteur sint occaecat velit</li>
                                        <li>Excepteur sint occaecat velit</li>
                                        <li>Excepteur sint occaecat velit</li>
                                    </ul>
                                </div>
                            </div>
                            <div class="pricing-item-cta mb-16">
                                <c-button tag="a" color="secondary" wide href="#">Free 14-day trial</c-button>
                            </div>
                        </div>
                    </div>

                    <div class="tiles-item reveal-from-top" data-reveal-delay="300">
                        <div class="tiles-item-inner has-shadow">
                            <div class="pricing-item-content">
                                <div class="pricing-item-header center-content mb-24">
                                    <div class="pricing-item-title text-xxs fw-500 pb-12">
                                        Business
                                    </div>
                                    <div class="pricing-item-price pt-24 pb-24">
                                        <span class="pricing-item-price-currency text-color-high">{{ getPricingData(this.priceOutput.plan2, 0) }}</span>
                                        <span class="pricing-item-price-amount h1">{{ getPricingData(this.priceOutput.plan2, 1) }}</span>/m
                                    </div>
                                </div>
                                <div class="pricing-item-features mb-40">
                                    <div class="pricing-item-features-title fw-500 text-xs text-color-high mb-24">
                                        Top features
                                    </div>
                                    <ul class="pricing-item-features-list list-reset text-xs">
                                        <li class="is-checked">Excepteur sint occaecat velit</li>
                                        <li class="is-checked">Excepteur sint occaecat velit</li>
                                        <li class="is-checked">Excepteur sint occaecat velit</li>
                                        <li class="is-checked">Excepteur sint occaecat velit</li>
                                        <li>Excepteur sint occaecat velit</li>
                                    </ul>
                                </div>
                            </div>
                            <div class="pricing-item-cta mb-16">
                                <c-button tag="a" color="primary" wide href="#">Free 14-day trial</c-button>
                            </div>
                        </div>
                    </div>

                    <div class="tiles-item reveal-from-top" data-reveal-delay="400">
                        <div class="tiles-item-inner has-shadow">
                            <div class="pricing-item-content">
                                <div class="pricing-item-header center-content mb-24">
                                    <div class="pricing-item-title text-xxs fw-500 pb-12">
                                        Enterprise
                                    </div>
                                    <div class="pricing-item-price pt-24 pb-24">
                                        <span class="pricing-item-price-currency text-color-high">{{ getPricingData(this.priceOutput.plan3, 0) }}</span>
                                        <span class="pricing-item-price-amount h1">{{ getPricingData(this.priceOutput.plan3, 1) }}</span>/m
                                    </div>
                                </div>
                                <div class="pricing-item-features mb-40">
                                    <div class="pricing-item-features-title fw-500 text-xs text-color-high mb-24">
                                        Top features
                                    </div>
                                    <ul class="pricing-item-features-list list-reset text-xs">
                                        <li class="is-checked">Excepteur sint occaecat velit</li>
                                        <li class="is-checked">Excepteur sint occaecat velit</li>
                                        <li class="is-checked">Excepteur sint occaecat velit</li>
                                        <li class="is-checked">Excepteur sint occaecat velit</li>
                                        <li class="is-checked">Excepteur sint occaecat velit</li>
                                    </ul>
                                </div>
                            </div>
                            <div class="pricing-item-cta mb-16">
                                <c-button tag="a" color="secondary" wide href="#">Free 14-day trial</c-button>
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
import CSwitch from '@/components/elements/Switch.vue'
import CButton from '@/components/elements/Button.vue'

export default {
  name: 'CPricing',
  components: {
    CSectionHeader,
    CSwitch,
    CButton
  },
  mixins: [SectionTilesProps],
  props: {
    pricingSwitcher: {
      type: Boolean,
      default: false
    },
    pricingSlider: {
      type: Boolean,
      default: false
    }        
  },
  data() {
    return {
      sectionHeader: {
        title: 'Supreme template pricing',
        paragraph:
          'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore aliqua.'
      },
      priceChangerValue: "1",
      priceInput: {
        // 0: "1,000",
        // 1: "1,250"
      },
      priceOutput: {
        plan1: {
          0: ["$", "35", "monthly"],
          1: ["$", "29", "annually"]
        },
        plan2: {
          0: ["$", "55", "monthly"],
          1: ["$", "49", "annually"]
        },
        plan3: {
          0: ["$", "95", "monthly"],
          1: ["$", "89", "annually"]
        }
      }
    }
  },
  methods: {    
    handlePricingSlide(e) {
      this.handleSliderValuePosition(e.target)
    },
    handleSliderValuePosition(input) {
      const multiplier = input.value / input.max
      const thumbOffset = this.thumbSize * multiplier
      const priceInputOffset = (this.thumbSize - this.$refs.sliderValue.clientWidth) / 2
      this.$refs.sliderValue.style.left = input.clientWidth * multiplier - thumbOffset + priceInputOffset + 'px'
    },
    getPricingData(values, set) {
      return set !== undefined ? values[this.priceChangerValue][set] : values[this.priceChangerValue]
    }
  },
  mounted() {
    if (this.pricingSlider) {
      this.$refs.slider.setAttribute('min', 0)
      this.$refs.slider.setAttribute('max', Object.keys(this.priceInput).length - 1)
      this.thumbSize = parseInt(window.getComputedStyle(this.$refs.sliderValue).getPropertyValue('--thumb-size'), 10)
      this.handleSliderValuePosition(this.$refs.slider)
    }        
  }
}
</script>