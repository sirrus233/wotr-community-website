import React from "react";
import Table from "@mui/joy/Table";

function Rankings() {
    return (
        <Table aria-label="basic table">
            <thead>
                <tr>
                    <th style={{ width: "8%" }}>Rank</th>
                    <th>Player</th>
                    <th>Avg. Rating</th>
                    <th>Shadow Rating</th>
                    <th>Free Rating</th>
                    <th>Games Played</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>1</td>
                    <td>Frodo</td>
                    <td>6</td>
                    <td>24</td>
                    <td>4</td>
                    <td>4</td>
                </tr>
                <tr>
                    <td>2</td>
                    <td>Sam</td>
                    <td>9</td>
                    <td>37</td>
                    <td>4.3</td>
                    <td>4</td>
                </tr>
                <tr>
                    <td>3</td>
                    <td>Merry</td>
                    <td>16</td>
                    <td>24</td>
                    <td>6</td>
                    <td>1</td>
                </tr>
                <tr>
                    <td>4</td>
                    <td>Pippin</td>
                    <td>3.7</td>
                    <td>67</td>
                    <td>4.3</td>
                    <td>6</td>
                </tr>
            </tbody>
        </Table>
    );
}

export default Rankings;
