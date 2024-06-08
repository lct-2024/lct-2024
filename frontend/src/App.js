import React, { useState } from "react"
import MainPage from "./components/Pages/MainPage";
import { BrowserRouter, Route, Routes } from "react-router-dom";
import VacancyPage from "./components/VacansyPage.jsx/VacansyPage";
import ProjectsPage from "./components/ProjectsPage/ProjectsPage";

function App() {

  return (
    <BrowserRouter>
      <div className="App">
        <Routes>
          <Route path="/" element={<MainPage />} />
          <Route path="/vacansy-page" element={<VacancyPage />} />
          <Route path="/projects-page" element={<ProjectsPage />} />
        </Routes>
      </div>
    </BrowserRouter>
  );
}

export default App;
