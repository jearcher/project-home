<template>
    <section
        class="hero section"
        :class="[
            topOuterDivider && 'has-top-divider',
            bottomOuterDivider && 'has-bottom-divider',
            hasBgColor && 'has-bg-color',
            invertColor && 'invert-color'
        ]">
        <div class="container">
            <div
                class="hero-inner section-inner"
                :class="[
                    topDivider && 'has-top-divider',
                    bottomDivider && 'has-bottom-divider'
                ]">
                <div class="hero-content invert-color center-content">
                    <div class="container-xs">
                        <h1 class="m-0 reveal-from-top">
                            Let us help find the right plan for you
                        </h1>
                    </div>
                </div>
                <div class="hero-figure pricing">
                    <div
                        class="tiles-wrap illustration-section-03 reveal-from-top"
                        :class="[
                        pushLeft && 'push-left',
                        ]"
                        data-reveal-delay="100">
                        <div class="tiles-item reveal-from-top" data-reveal-delay="200">
                            <div class="tiles-item-inner has-shadow">
                                <div class="pricing-item-content">
                                    <div class="pricing-item-header center-content mb-24">
                                        <div class="pricing-item-title text-xxs fw-500 pb-12">
                                            Essential
                                        </div>
                                        <div class="pricing-item-price pt-24 pb-24">
                                            <span class="pricing-item-price-currency text-color-high">$</span>
                                            <span class="pricing-item-price-amount h1">29</span>/m
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
                                            <span class="pricing-item-price-currency text-color-high">$</span>
                                            <span class="pricing-item-price-amount h1">49</span>/m
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
                                            <span class="pricing-item-price-currency text-color-high">$</span>
                                            <span class="pricing-item-price-amount h1">89</span>/m
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
        </div>
    </section>
</template>

<script>
import { SectionTilesProps } from '@/utils/SectionProps.js'
import CButton from '@/components/elements/Button.vue'

export default {
  name: 'CHeroFull',
  mixins: [SectionTilesProps],
  components: {
    CButton
  }
}
</script>