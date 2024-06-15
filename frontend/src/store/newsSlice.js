import { createSlice, createAsyncThunk } from "@reduxjs/toolkit";
import axios from 'axios';

export const fetchNews = createAsyncThunk(
    'news/fetchNews',
    async () => {
        const response = await axios.post('https://ats.lct24.dev.40ants.com/api/get_news', {
            jsonrpc: '2.0',
            method: 'get_news',
            params: [],
            id: 1
        });

        const newsData = response.data.result;
        console.log(newsData)
        return newsData;
    }
);

const newsSlice = createSlice({
    name: 'news',
    initialState: {
        data: [],
        status: 'idle',
        error: null
    },
    reducers: {
        setNews: (state, action) => {
            state.data = action.payload;
        }
    },
    extraReducers: (builder) => {
        builder
            .addCase(fetchNews.pending, (state) => {
                state.status = 'loading';
            })
            .addCase(fetchNews.fulfilled, (state, action) => {
                state.status = 'succeeded';
                state.data = action.payload;
            })
            .addCase(fetchNews.rejected, (state, action) => {
                state.status = 'failed';
                state.error = action.error.message;
            });
    }
});

export const { setNews } = newsSlice.actions;
export default newsSlice.reducer;
