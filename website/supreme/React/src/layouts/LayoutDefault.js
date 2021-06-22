import React from 'react';
import Header from '../components/layout/Header';
import Footer from '../components/layout/Footer';

const LayoutDefault = ({ children }) => (
  <React.Fragment>
    <Header navPosition="right" />
    <main className="site-content">
      {children}
    </main>
    <Footer topDivider className="illustration-section-08" />
  </React.Fragment>
);

export default LayoutDefault;  