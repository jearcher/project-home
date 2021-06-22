import React from 'react';
import classNames from 'classnames';
import { SectionTilesProps } from '../../utils/SectionProps';
import SectionHeader from './partials/SectionHeader';
import Image from '../elements/Image';

const propTypes = {
  ...SectionTilesProps.types
}

const defaultProps = {
  ...SectionTilesProps.defaults
}

class Testimonial extends React.Component {

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
      'testimonial section center-content',
      topOuterDivider && 'has-top-divider',
      bottomOuterDivider && 'has-bottom-divider',
      hasBgColor && 'has-bg-color',
      invertColor && 'invert-color',
      className
    );

    const innerClasses = classNames(
      'testimonial-inner section-inner',
      topDivider && 'has-top-divider',
      bottomDivider && 'has-bottom-divider'
    );

    const tilesClasses = classNames(
      'tiles-wrap illustration-section-04 reveal-from-top',
      pushLeft && 'push-left'
    );

    const sectionHeader = {
      title: 'Customer Testimonials',
      paragraph: ''
    };

    return (
      <section
        {...props}
        className={outerClasses}
      >
        <div className="container">
          <div className={innerClasses}>
            <SectionHeader data={sectionHeader} className="center-content" />
            <div className={tilesClasses}>

              <div className="tiles-item reveal-from-top" data-reveal-delay="200">
                <div className="tiles-item-inner has-shadow">
                  <div className="testimonial-item-header">
                    <div className="testimonial-item-image mt-8 mb-24">
                      <Image
                        src={require('./../../assets/images/testimonial-image-01.png')}
                        alt="Testimonial 01"
                        width={56}
                        height={56} />
                    </div>
                  </div>
                  <div className="testimonial-item-content">
                    <p className="text-sm mb-24">
                      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
                      Ut enim ad minim veniam, quis nostrud labore et dolore magna aliqua.
                    </p>
                  </div>
                  <div className="testimonial-item-footer has-top-divider">
                    <div className="testimonial-item-name h6 m-0">
                      Erika Muliawan
                    </div>
                    <div className="testimonial-item-link text-xs">
                      <a href="#0">AppName</a>
                    </div>
                  </div>
                </div>
              </div>

              <div className="tiles-item reveal-from-top" data-reveal-delay="300">
                <div className="tiles-item-inner has-shadow">
                  <div className="testimonial-item-header">
                    <div className="testimonial-item-image mt-8 mb-24">
                      <Image
                        src={require('./../../assets/images/testimonial-image-02.png')}
                        alt="Testimonial 02"
                        width={56}
                        height={56} />
                    </div>
                  </div>
                  <div className="testimonial-item-content">
                    <p className="text-sm mb-24">
                      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
                      Ut enim ad minim veniam, quis nostrud labore et dolore magna aliqua.
                    </p>
                  </div>
                  <div className="testimonial-item-footer has-top-divider">
                    <div className="testimonial-item-name h6 m-0">
                      Erika Muliawan
                    </div>
                    <div className="testimonial-item-link text-xs">
                      <a href="#0">AppName</a>
                    </div>
                  </div>
                </div>
              </div>

              <div className="tiles-item reveal-from-top">
                <div className="tiles-item-inner has-shadow">
                  <div className="testimonial-item-header">
                    <div className="testimonial-item-image mt-8 mb-24">
                      <Image
                        src={require('./../../assets/images/testimonial-image-03.png')}
                        alt="Testimonial 03"
                        width={56}
                        height={56} />
                    </div>
                  </div>
                  <div className="testimonial-item-content">
                    <p className="text-sm mb-24">
                      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
                      Ut enim ad minim veniam, quis nostrud labore et dolore magna aliqua.
                    </p>
                  </div>
                  <div className="testimonial-item-footer has-top-divider">
                    <div className="testimonial-item-name h6 m-0">
                      Erika Muliawan
                    </div>
                    <div className="testimonial-item-link text-xs">
                      <a href="#0">AppName</a>
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

Testimonial.propTypes = propTypes;
Testimonial.defaultProps = defaultProps;

export default Testimonial;