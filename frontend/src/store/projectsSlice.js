import { createSlice } from "@reduxjs/toolkit";

const projectsSlise = createSlice({
    name: "projects",
    initialState: JSON.parse(localStorage.getItem("projects")) || [],
    reducers: {
        setProjects: (state, action) => {
            return action.payload;
        },
    }
})

export const { setProjects } = projectsSlise.actions
export default projectsSlise.reducer