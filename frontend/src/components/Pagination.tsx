import React, { useState } from "react";
import Box from "@mui/joy/Box";
import ChevronLeft from "@mui/icons-material/ChevronLeft";
import ChevronRight from "@mui/icons-material/ChevronRight";
import IconButton from "@mui/joy/IconButton";
import { range } from "../utils";

interface Props {
    currentPage: number;
    setCurrentPage: (page: number) => void;
    totalCount: number;
    perPageCount: number;
    slots?: number;
    minSpanEachSide?: number;
}

export default function Pagination({
    currentPage,
    setCurrentPage,
    totalCount,
    perPageCount,
    slots = 7,
    minSpanEachSide = 2,
}: Props) {
    const pageCount = Math.ceil(totalCount / perPageCount);
    const pageRanges = toPageRanges(
        currentPage,
        pageCount,
        slots,
        minSpanEachSide
    );

    return (
        <Box
            display="flex"
            flexDirection="row"
            alignItems="center"
            justifyContent="center"
            width="100%"
            overflow="auto"
        >
            <IconButton
                disabled={currentPage === 1}
                onClick={() => setCurrentPage(currentPage - 1)}
                variant="plain"
            >
                <ChevronLeft />
            </IconButton>

            {pageRanges.map((pageRange, i) => (
                <React.Fragment key={pageRange.join()}>
                    {pageRange.map((page) => (
                        <PageButton
                            key={page}
                            value={page}
                            onClick={() => setCurrentPage(page)}
                            selected={currentPage === page}
                        />
                    ))}
                    {i !== pageRanges.length - 1 && <PageButton value="..." />}
                </React.Fragment>
            ))}

            <IconButton
                disabled={currentPage === pageCount}
                onClick={() => setCurrentPage(currentPage + 1)}
                variant="plain"
            >
                <ChevronRight />
            </IconButton>
        </Box>
    );
}

interface PageButtonProps {
    value: number | string;
    selected?: boolean;
    onClick?: () => void;
}

function PageButton({ value, selected, onClick }: PageButtonProps) {
    return (
        <IconButton
            disabled={!onClick}
            onClick={onClick}
            variant={selected ? "solid" : "plain"}
            color={selected ? "primary" : "neutral"}
            sx={{ mx: "1px" }}
        >
            {value}
        </IconButton>
    );
}

function toPageRanges(
    current: number,
    final: number,
    slots: number,
    minSpanEachSide: number
): number[][] {
    const oneSidedSpan = slots - minSpanEachSide;
    const endThreshold = final - oneSidedSpan + 1;

    const middleSpanExcludingCurrent = slots - minSpanEachSide * 2 - 1;
    const leftSpan = Math.ceil(middleSpanExcludingCurrent / 2);
    const rightSpan = Math.floor(middleSpanExcludingCurrent / 2);

    if (
        final > slots &&
        current > 0 &&
        current <= final &&
        minSpanEachSide * 2 <= final
    ) {
        if (current < oneSidedSpan) {
            return [range(1, oneSidedSpan + 1), [final]];
        } else if (current > endThreshold) {
            return [[1], range(endThreshold, final + 1)];
        } else {
            return [
                [1],
                range(current - leftSpan, current + rightSpan + 1),
                [final],
            ];
        }
    }

    return [range(1, final + 1)];
}
