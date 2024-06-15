import { createSlice, createAsyncThunk } from "@reduxjs/toolkit";
import axios from 'axios';

export const fetchVacansies = createAsyncThunk(
    'vacansies/fetchVacansies',
    async () => {
        const response = await axios.post('https://ats.lct24.dev.40ants.com/api/get_jobs', {
            jsonrpc: '2.0',
            method: 'get_jobs',
            params: [],
            id: 1
        });
        console.log(response.data)
        return response.data.result;
    }
);

const vacansiesSlice = createSlice({
    name: 'vacansies',
    initialState: {
        data: [],
        status: 'idle',
        error: null
    },
    reducers: {
        setVacansies: (state, action) => {
            state.data = action.payload;
        }
    },
    extraReducers: (builder) => {
        builder
            .addCase(fetchVacansies.pending, (state) => {
                state.status = 'loading';
            })
            .addCase(fetchVacansies.fulfilled, (state, action) => {
                state.status = 'succeeded';
                state.data = action.payload;
            })
            .addCase(fetchVacansies.rejected, (state, action) => {
                state.status = 'failed';
                state.error = action.error.message;
            });
    }
});

export const { setVacansies } = vacansiesSlice.actions;
export default vacansiesSlice.reducer;
