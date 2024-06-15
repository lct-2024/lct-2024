import React from 'react';
import ReactDOM from 'react-dom/client';
import './index.css';
import App from './App';
import reportWebVitals from './reportWebVitals';
import { Provider } from 'react-redux';
import { configureStore } from '@reduxjs/toolkit';
import projectsReducer from "./store/projectsSlice"
import vacansiesReducer from "./store/vacansiesSlise"
import authReducer from './store/authSlice'
import newsReducer from './store/newsSlice'
import commentsReducer from './store/commentsSlice';
import resumeReducer from './store/resumeSlice';
import { BrowserRouter } from 'react-router-dom';

const store = configureStore({
  reducer: {
    projects: projectsReducer,
    vacansies: vacansiesReducer,
    auth: authReducer,
    news: newsReducer,
    comments: commentsReducer,
    resume: resumeReducer,
  },
});

const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(
  <React.StrictMode>
    <Provider store={store}>
      <BrowserRouter>
        <App />
      </BrowserRouter>
    </Provider>
  </React.StrictMode>
);

reportWebVitals();
