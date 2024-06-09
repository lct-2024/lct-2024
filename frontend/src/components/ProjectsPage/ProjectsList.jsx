import React, { useState } from 'react'
import style from './ProjectsList.module.css'

const ProjectsList = ({ projects }) => {

    return (
        <div className='container'>
            <div className={style.list}>
                {projects.map((project, i) => {
                    return <div className={style.project} key={i}>
                        <div>
                            <h2>{project.title}</h2>
                            <svg width="24" height="25" viewBox="0 0 24 25" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M10.3 21.5C10.4674 21.8044 10.7134 22.0583 11.0125 22.2352C11.3115 22.412 11.6526 22.5053 12 22.5053C12.3474 22.5053 12.6885 22.412 12.9875 22.2352C13.2866 22.0583 13.5326 21.8044 13.7 21.5M6 8.5C6 6.9087 6.63214 5.38258 7.75736 4.25736C8.88258 3.13214 10.4087 2.5 12 2.5C13.5913 2.5 15.1174 3.13214 16.2426 4.25736C17.3679 5.38258 18 6.9087 18 8.5C18 15.5 21 17.5 21 17.5H3C3 17.5 6 15.5 6 8.5Z" stroke="black" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                            </svg>
                        </div>
                        <p>{project.description}</p>
                        <p>Открытых вакансий: {project.count}</p>
                        <button>Другое</button>
                    </div>
                })}
            </div>
            <button className={style.btn}>Загрузить ещё</button>
        </div>
    )
}

export default ProjectsList