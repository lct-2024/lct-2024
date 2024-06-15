import React, { useEffect, useState } from 'react';
import style from './ProjectsPage.module.css';
import Navigation from '../Navigation';
import ProjectsList from './ProjectsList';
import Footer from '../Footer';
import axios from 'axios';
import { useDispatch, useSelector } from 'react-redux';
import { setProjects } from '../../store/projectsSlice';
import Comments from '../Comments';

const ProjectsPage = () => {

    const projects = useSelector(state => state.projects)
    const dispatch = useDispatch()
    const [filteredProjects, setFilteredProjects] = useState([]);
    const [searchTerm, setSearchTerm] = useState('');
    const [selectedFilter, setSelectedFilter] = useState('Все');

    useEffect(() => {
        const fetchData = async () => {
            try {
                const response = await axios.post('https://ats.lct24.dev.40ants.com/api/get_projects', {
                    jsonrpc: '2.0',
                    method: 'get_projects',
                    params: [],
                    id: 1
                });
                if (response.data.error) {
                    console.error('Error fetching data:', response.data.error.message);
                } else {
                    dispatch(setProjects(response.data.result));
                    setFilteredProjects(response.data.result);
                }
            } catch (error) {
                console.error('Error fetching data:', error);
            }
        };

        fetchData();
    }, []);

    useEffect(() => {
        filterProjects();
    }, [searchTerm, selectedFilter, projects]);

    const filterProjects = () => {
        let filtered = [...projects];

        if (searchTerm.trim() !== '') {
            filtered = filtered.filter((project) =>
                project.title.toLowerCase().includes(searchTerm.toLowerCase())
            );
        }

        if (selectedFilter !== 'Все') {
            filtered = filtered.filter((project) =>
                project.themes.some((theme) => theme.title.toLowerCase() === selectedFilter.toLowerCase())
            );
        }

        setFilteredProjects(filtered);
    };

    const handleSearchChange = (event) => {
        setSearchTerm(event.target.value);
    };

    const handleFilterClick = (filter) => {
        setSelectedFilter(filter);
    };

    const handleKeyPress = (event) => {
        if (event.key === 'Enter') {
            filterProjects();
        }
    };

    return (
        <>
            <div className={style.main}>
                <div className='container'>
                    <div className={style.head}>
                        <Navigation />
                        <h1>ПРОЕКТЫ</h1>
                    </div>
                </div>
            </div>
            <div className='container'>
                <div className={filteredProjects.length === 0 ? style.body2 : style.body}>
                    <div>
                        <div className={style.search}>
                            <input
                                type="text"
                                placeholder='Поиск...'
                                value={searchTerm}
                                onChange={handleSearchChange}
                                onKeyPress={handleKeyPress}
                            />
                        </div>
                        <div className={style.filter}>
                            {['Все', 'Финтех', 'Госсектор', 'IT', 'Медиа', 'Нефть и газ', 'Ретейл', 'Коммуникации', 'Транспорт', 'Другое'].map((filter) => (
                                <p
                                    key={filter}
                                    className={selectedFilter === filter ? style.activeFilter : ''}
                                    onClick={() => handleFilterClick(filter)}
                                >
                                    {filter}
                                </p>
                            ))}
                        </div>
                    </div>
                    {filteredProjects.length === 0 ? (
                        <h1 style={{ margin: '0px auto' }}>Проектов нет</h1>
                    ) : (
                        <ProjectsList projects={filteredProjects} />
                    )}
                    <Comments text="проектах" />
                </div>
            </div>
            <Footer />
        </>
    );
};

export default ProjectsPage;
