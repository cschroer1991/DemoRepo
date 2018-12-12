/*------------------------------------------------------------------------
    File        : tripreq
    Purpose     : Class file to handle all Trip Request processing
    Author(s)   : Zak Hurley
    Created     : Fri Sep 08 14:49:00 EDT 2017
    Notes       : Created for TIWO 74788
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

class class.tripreq: 
    define private static buffer TripReq for TripReq.
    define private static buffer inv for inv.
    
    define private static temp-table tParts
        field iPart as integer.
    
    /*------------------------------------------------------------------------------
     Purpose: Find appropriate Trip Request (if any) and adjust weights. 
              Close Request if needed.
    ------------------------------------------------------------------------------*/
    method static public void AdjustTripReq( input piPart as integer,
                                      input piWt as integer,
                                      input piToPartner as integer,
                                      input piToPart as integer):
        for each TripReq where TripReq.stat < 2
            and TripReq.FromPartID = piPart:
            if TripReq.ToPartnerID > 0 and TripReq.ToPartnerID <> piToPartner then
                next.
            if TripReq.ToPartID > 0 and TripReq.ToPartID <> piToPart then
                next.
            TripReq.TripWt = TripReq.TripWt + piWt.
            if TripReq.TripWt < TripReq.ReqWt then
                TripReq.stat = 1. /*In Process*/
            else 
                assign
                    TripReq.CompDate = today
                    TripReq.stat = 2. /*Completed*/
        end. /*for each TripReq*/
        return.
    end method. /*AdjustTripReq*/
end class.