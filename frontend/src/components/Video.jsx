import React from 'react'
import style from "./Video.module.css"

const Video = ({ title, text, link }) => {
    return (
        <div className={style.main}>
            <div className="container">
                <div className={style.sect}>
                    <div>
                        <h2>{title}</h2>
                        <p>{text}</p>
                        <a href={link} target='_blank'>{link}</a>
                    </div>
                    <video src={link} controls ></video>
                </div>
            </div>
        </div>
    )
}

export default Video