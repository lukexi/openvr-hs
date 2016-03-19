import Graphics.VR.OpenVR

main = do
    isPresent <- isHMDPresent
    mOpenVR <- if isPresent then initOpenVR else return Nothing

    print mOpenVR