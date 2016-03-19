import Graphics.VR.OpenVR

main :: IO ()
main = do
    isPresent <- isHMDPresent
    mOpenVR <- if isPresent then initOpenVR else return Nothing

    print mOpenVR