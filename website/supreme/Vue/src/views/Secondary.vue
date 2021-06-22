<template>
    <fragment>
        <c-hero-pricing has-bg-color pricing-switcher class="illustration-section-01" />
        <c-team />
        <c-clients top-divider bottom-divider />

        <c-generic-section>
            <div class="container-xs">
                <h2 class="mt-0">Lorem ipsum is placeholder text commonly used in the graphic.</h2>
                <p>
                    Lorem ipsum dolor sit amet, <a href="#">consectetur adipiscing elit</a>, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                </p>
                <p>
                    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
                </p>
                <figure>
                    <c-image
                        class="image-larger"
                        :src="require('@/assets/images/image-placeholder.png')"
                        alt="Image placeholder"
                        :width="712"
                        :height="400" />
                    <figcaption class="text-color-low">A super-nice image <span role="img" aria-label="smile">😀</span></figcaption>
                </figure>
                <h4>Flexibility</h4>
                <p>
                    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
                </p>
                <ul>
                    <li>Lorem ipsum dolor sit amet, consectetur.</li>
                    <li>Lorem ipsum dolor sit amet, consectetur.</li>
                    <li>Lorem ipsum dolor sit amet, consectetur.</li>
                </ul>
                <p>
                    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris.
                </p>
            </div>
        </c-generic-section>

        <c-generic-section top-divider class="center-content">
            <div class="container">
                <c-section-header :data="genericSection01Header" class="center-content" />
                <c-button-group>
                    <c-button color="primary" wide-mobile>Get started</c-button>
                    <c-button color="secondary" wide-mobile>Get started</c-button>
                    <c-button color="dark" wide-mobile>Get started</c-button>
                </c-button-group>
            </div>
        </c-generic-section>

        <c-generic-section top-divider>
            <div class="container-xs">
                <c-section-header :data="genericSection02Header" class="center-content" />
                <c-accordion>
                    <c-accordion-item title="Nisi porta lorem mollis aliquam ut." active>
                        Lorem ipsum is placeholder text commonly used in the graphic, print, and publishing industries for previewing layouts and visual mockups.
                    </c-accordion-item>
                    <c-accordion-item title="Nisi porta lorem mollis aliquam ut.">
                        Lorem ipsum is placeholder text commonly used in the graphic, print, and publishing industries for previewing layouts and visual mockups.
                    </c-accordion-item>
                    <c-accordion-item title="Nisi porta lorem mollis aliquam ut.">
                        Lorem ipsum is placeholder text commonly used in the graphic, print, and publishing industries for previewing layouts and visual mockups.
                    </c-accordion-item>
                    <c-accordion-item title="Nisi porta lorem mollis aliquam ut.">
                        Lorem ipsum is placeholder text commonly used in the graphic, print, and publishing industries for previewing layouts and visual mockups.
                    </c-accordion-item>
                    <c-accordion-item title="Nisi porta lorem mollis aliquam ut.">
                        Lorem ipsum is placeholder text commonly used in the graphic, print, and publishing industries for previewing layouts and visual mockups.
                    </c-accordion-item>
                </c-accordion>
            </div>
        </c-generic-section>

        <c-generic-section top-divider>
            <div class="container-xs">
                <c-section-header :data="genericSection03Header" class="center-content" />
                <form style="max-width:420px;margin:0 auto;">
                    <div class="mb-24">
                        <c-input
                            type="email"
                            label="This is a label"
                            placeholder="Your best email.."
                            form-group="desktop"
                            label-hidden>
                            <c-button color="primary">Early access</c-button>
                        </c-input>
                    </div>
                    <div class="mb-24">
                        <c-input
                            type="email"
                            label="This is a label"
                            placeholder="Your best email.."
                            form-group="desktop"
                            label-hidden
                            status="error"
                            hint="Something is wrong.">
                            <c-button color="primary">Early access</c-button>
                        </c-input>
                    </div>
                    <div class="mb-24">
                        <c-input
                            type="email"
                            label="This is a label"
                            placeholder="Your best email.."
                            form-group="desktop"
                            label-hidden
                            status="success"
                            hint="You've done it.">
                            <c-button color="primary">Early access</c-button>
                        </c-input>
                    </div>
                </form>
            </div>
        </c-generic-section>

        <c-generic-section top-divider>
            <div class="container-xs">
                <c-section-header :data="genericSection04Header" class="center-content" />
                <div class="center-content">
                    <c-button
                        color="secondary"
                        aria-controls="demo-modal"
                        @click="demoModalActive = true">Open modal</c-button>
                </div>
                <c-modal id="demo-modal" :active.sync="demoModalActive">
                    <div class="center-content">
                        <h3 class="mt-0 mb-12">Nisi porta lorem mollis ut.</h3>
                        <p class="mb-32">
                            Magnis dis parturient montes nascetur. Quam quisque id diam vel quam ultricies leo integer.
                        </p>
                    </div>
                    <div>
                        <c-input
                            type="email"
                            label="This is a label"
                            placeholder="Your best email.."
                            form-group="desktop"
                            label-hidden>
                            <c-button color="primary">Subscribe</c-button>
                        </c-input>
                    </div>
                </c-modal>
            </div>
        </c-generic-section>

        <c-generic-section top-divider class="center-content">
            <div class="container-xs">
                <c-section-header :data="genericSection05Header" class="center-content" />
            </div>
            <div class="container-sm">
                <a
                    data-video="https://player.vimeo.com/video/174002812"
                    href="#"
                    aria-controls="video-modal"
                    @click.prevent="videoModalActive = true">
                    <c-image
                        class="has-shadow"
                        :src="require('@/assets/images/video-placeholder.png')"
                        alt="Video"
                        :width="712"
                        :height="400" />
                </a>
                <c-modal
                    id="video-modal"
                    :active.sync="videoModalActive"
                    video="https://player.vimeo.com/video/174002812"
                    video-tag="iframe" />
            </div>
        </c-generic-section>

        <c-roadmap top-divider />
        <c-cta has-bg-color invert-color split />
    </fragment>
</template>

<script>
// import layout
import CLayout from '@/layouts/LayoutAlternative.vue'
// import section header
import CSectionHeader from '@/components/sections/partials/SectionHeader.vue'
// import sections
import CHeroPricing from '@/components/sections/HeroPricing.vue'
import CClients from '@/components/sections/Clients.vue'
import CTeam from '@/components/sections/Team.vue'
import CGenericSection from '@/components/sections/GenericSection.vue'
import CRoadmap from '@/components/sections/Roadmap.vue'
import CCta from '@/components/sections/Cta.vue'
// import some required elements
import CImage from '@/components/elements/Image.vue'
import CInput from '@/components/elements/Input.vue'
import CButtonGroup from '@/components/elements/ButtonGroup.vue'
import CButton from '@/components/elements/Button.vue'
import CModal from '@/components/elements/Modal.vue'
import CAccordion from '@/components/elements/Accordion.vue'
import CAccordionItem from '@/components/elements/AccordionItem.vue'

export default {
  name: 'Secondary',
  components: {
    CSectionHeader,
    CHeroPricing,
    CClients,
    CTeam,
    CGenericSection,
    CImage,
    CRoadmap,
    CCta,
    CInput,
    CButtonGroup,
    CButton,
    CModal,
    CAccordion,
    CAccordionItem
  },
  data() {
    return {
      genericSection01Header: {
        title: 'Buttons - Lorem ipsum is placeholder text commonly used.'
      },
      genericSection02Header: {
        title: 'FAQ - Lorem ipsum is placeholder text commonly used.'
      },
      genericSection03Header: {
        title: 'Input forms - Lorem ipsum is placeholder text commonly used.'
      },
      genericSection04Header: {
        title: 'Modal - Lorem ipsum is placeholder text commonly used.'
      },
      genericSection05Header: {
        title: 'Video - Lorem ipsum is placeholder text commonly used.'
      },
      demoModalActive: false,
      videoModalActive: false
    }
  },
  created() {
    this.$emit('update:layout', CLayout)
  }
}
</script>
