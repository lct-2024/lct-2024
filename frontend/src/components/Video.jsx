import React from 'react'
import style from "./Video.module.css"

const Video = ({ title, text, link }) => {

    let t;

    return (
        <div className='container'>
            <div className={t ? style.main : style.main2}>
                <div className={style.sect}>
                    <div>
                        <h2>{title}</h2>
                        <p>{text}</p>
                    </div>
                    {link == "https://www.youtube.com/watch?v=fI4htZchBz0" ?
                        <iframe width="300px" className={style.video} height="400" src="https://www.youtube.com/embed/fI4htZchBz0?si=8GaEWZP1_nu65e8O" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>
                        :
                        t = true && <iframe width="400" height="400" className={style.video} src="https://www.youtube.com/embed/9pZLCAIEUHM?si=pNeh2RYK9Rgv5AGh" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>
                    }</div>
            </div>
        </div>
    )
}

export default Video