import React, { useEffect } from "react";
import { BrowserRouter, Link, Route, Routes } from "react-router-dom";
import { useDispatch, useSelector } from "react-redux";
import { setAuthToken, setUser } from "./store/authSlice";
import MainPage from "./components/Pages/MainPage";
import VacancyPage from "./components/VacansyPage/VacansyPage";
import ProjectsPage from "./components/ProjectsPage/ProjectsPage";
import Login from "./components/AuthPage/Login";
import Registration from "./components/AuthPage/Registration";
import VacansyInfo from "./components/VacansyPage/VacansyInfo";
import UserProfile from "./components/UserProfile/UserProfile";
import NewsPage from "./components/NewsPage/NewsPage";

function App() {
  const dispatch = useDispatch();
  const user = useSelector((state) => state.auth.user);

  useEffect(() => {
    const token = localStorage.getItem('authToken');
    const storedUser = localStorage.getItem('user');

    if (token && storedUser) {
      dispatch(setAuthToken(token));
      dispatch(setUser(JSON.parse(storedUser)));
    }
  }, [dispatch]);

  return (
    <div className="App">
      <Routes>
        <Route path="/" element={<MainPage />} />
        <Route path="/vacansy-page" element={<VacancyPage />} />
        <Route path="/vacansy-info/:id" element={<VacansyInfo />} />
        <Route path="/projects-page" element={<ProjectsPage />} />
        <Route path="/login" element={<Login />} />
        <Route path="/registration" element={<Registration />} />
        {user && <Route path="/profile" element={<UserProfile />} />}
        <Route path="/news" element={<NewsPage />} />
        <Route path="*" element={<h1 style={{ margin: "auto" }}>Вернитесь на <Link to="/">главную страницу</Link></h1>} />
      </Routes>
    </div>
  );
}

export default App;
