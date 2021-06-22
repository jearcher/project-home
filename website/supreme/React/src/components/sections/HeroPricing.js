import React from 'react';
import classNames from 'classnames';
import { SectionTilesProps } from '../../utils/SectionProps';
import Button from '../elements/Button';

const propTypes = {
  ...SectionTilesProps.types
}

const defaultProps = {
  ...SectionTilesProps.defaults
}

class HeroFull extends React.Component {

  render() {
    const {
      className,
      topOuterDivider,
      bottomOuterDivider,      
      topDivider,
      bottomDivider,
      hasBgColor,
      invertColor,
      pushLeft,
      ...props
    } = this.props;

    const outerClasses = classNames(
      'hero section',
      topOuterDivider && 'has-top-divider',
      bottomOuterDivider && 'has-bottom-divider',
      hasBgColor && 'has-bg-color',
      invertColor && 'invert-color',
      className
    );

    const innerClasses = classNames(
      'hero-inner section-inner',
      topDivider && 'has-top-divider',
      bottomDivider && 'has-bottom-divider'
    );

    const tilesClasses = classNames(
      'tiles-wrap illustration-section-03 reveal-from-top',
      pushLeft && 'push-left'
    );

    return (
      <section
        {...props}
        className={outerClasses}
      >
        <div className="container">
          <div className={innerClasses}>
            <div className="hero-content invert-color center-content">
              <div className="container-xs">
                <h1 className="m-0 reveal-from-top">
                  Let us help find the right plan for you
                </h1>
              </div>
            </div>
            <div className="hero-figure pricing">
              <div className={tilesClasses} data-reveal-delay="100">

                <div className="tiles-item reveal-from-top" data-reveal-delay="200">
                  <div className="tiles-item-inner has-shadow">
                    <div className="pricing-item-content">
                      <div className="pricing-item-header center-content mb-24">
                        <div className="pricing-item-title text-xxs fw-500 pb-12">
                          Essential
                        </div>
                        <div className="pricing-item-price pt-24 pb-24">
                          <span className="pricing-item-price-currency text-color-high">$</span>
                          <span className="pricing-item-price-amount h1">29</span>/m
                        </div>
                      </div>
                      <div className="pricing-item-features mb-40">
                        <div className="pricing-item-features-title fw-500 text-xs text-color-high mb-24">
                          Top features
                        </div>
                        <ul className="pricing-item-features-list list-reset text-xs">
                          <li className="is-checked">Excepteur sint occaecat velit</li>
                          <li className="is-checked">Excepteur sint occaecat velit</li>
                          <li className="is-checked">Excepteur sint occaecat velit</li>
                          <li>Excepteur sint occaecat velit</li>
                          <li>Excepteur sint occaecat velit</li>
                        </ul>
                      </div>
                    </div>
                    <div className="pricing-item-cta mb-16">
                      <Button tag="a" color="secondary" wide href="http://cruip.com/">Free 14-day trial</Button>
                    </div>
                  </div>
                </div>

                <div className="tiles-item reveal-from-top" data-reveal-delay="300">
                  <div className="tiles-item-inner has-shadow">
                    <div className="pricing-item-content">
                      <div className="pricing-item-header center-content mb-24">
                        <div className="pricing-item-title text-xxs fw-500 pb-12">
                          Business
                        </div>
                        <div className="pricing-item-price pt-24 pb-24">
                          <span className="pricing-item-price-currency text-color-high">$</span>
                          <span className="pricing-item-price-amount h1">49</span>/m
                        </div>
                      </div>
                      <div className="pricing-item-features mb-40">
                        <div className="pricing-item-features-title fw-500 text-xs text-color-high mb-24">
                          Top features
                        </div>
                        <ul className="pricing-item-features-list list-reset text-xs">
                          <li className="is-checked">Excepteur sint occaecat velit</li>
                          <li className="is-checked">Excepteur sint occaecat velit</li>
                          <li className="is-checked">Excepteur sint occaecat velit</li>
                          <li className="is-checked">Excepteur sint occaecat velit</li>
                          <li>Excepteur sint occaecat velit</li>
                        </ul>
                      </div>
                    </div>
                    <div className="pricing-item-cta mb-16">
                      <Button tag="a" color="primary" wide href="http://cruip.com/">Free 14-day trial</Button>
                    </div>
                  </div>
                </div>

                <div className="tiles-item reveal-from-top" data-reveal-delay="300">
                  <div className="tiles-item-inner has-shadow">
                    <div className="pricing-item-content">
                      <div className="pricing-item-header center-content mb-24">
                        <div className="pricing-item-title text-xxs fw-500 pb-12">
                          Enterprise
                        </div>
                        <div className="pricing-item-price pt-24 pb-24">
                          <span className="pricing-item-price-currency text-color-high">$</span>
                          <span className="pricing-item-price-amount h1">89</span>/m
                        </div>
                      </div>
                      <div className="pricing-item-features mb-40">
                        <div className="pricing-item-features-title fw-500 text-xs text-color-high mb-24">
                          Top features
                        </div>
                        <ul className="pricing-item-features-list list-reset text-xs">
                          <li className="is-checked">Excepteur sint occaecat velit</li>
                          <li className="is-checked">Excepteur sint occaecat velit</li>
                          <li className="is-checked">Excepteur sint occaecat velit</li>
                          <li className="is-checked">Excepteur sint occaecat velit</li>
                          <li className="is-checked">Excepteur sint occaecat velit</li>
                        </ul>
                      </div>
                    </div>
                    <div className="pricing-item-cta mb-16">
                      <Button tag="a" color="secondary" wide href="http://cruip.com/">Free 14-day trial</Button>
                    </div>
                  </div>
                </div>

              </div>
            </div>
          </div>
        </div>
      </section>
    );
  }
}

HeroFull.propTypes = propTypes;
HeroFull.defaultProps = defaultProps;

export default HeroFull;