module UnderlyingFS where

import System.IO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath)

newtype Handles = Handles [(Handle, Integer)]  -- ^[(fileHandle, fileLength)]

projectHandles :: Handles
               -> Integer    -- ^Torrent offset
               -> Integer    -- ^Torrent size
               -> [(Handle
                   ,Integer
                   ,Integer
                   )]        -- ^ (File handle, file chunk offset, file chunk size)
{-
projectHandles handles offset size = let r = projectHandles' handles offset size
                                     in trace ("projectHandles " ++
                                               show handles ++ " " ++
                                               show offset ++ " " ++
                                               show size ++ " = " ++
                                               show r
                                              ) $
                                        r
-}
projectHandles (Handles handles@((h1, length1):handles')) offs size
    | size <= 0 =
        fail "FS: Should have already stopped projection"
    | null handles =
        fail "FS: Attempt to read beyond torrent length"
    | offs >= length1 =
        projectHandles (Handles handles') (offs - length1) size
    | otherwise =
        let size1 = length1 - offs  -- How much of h1 to take?
        in if size1 >= size
           then [(h1, offs, size)]
           else (h1, offs, size1) :
                projectHandles (Handles handles') 0 (size - size1)
projectHandles (Handles []) _ _ = fail "FS: Empty Handles list, can't happen"

