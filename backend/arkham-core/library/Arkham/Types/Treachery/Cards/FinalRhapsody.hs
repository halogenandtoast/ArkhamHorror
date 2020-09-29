{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.FinalRhapsody where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype FinalRhapsody = FinalRhapsody Attrs
  deriving newtype (Show, ToJSON, FromJSON)

finalRhapsody :: TreacheryId -> Maybe InvestigatorId -> FinalRhapsody
finalRhapsody uuid iid = FinalRhapsody $ weaknessAttrs uuid iid "02013"

instance HasActions env investigator FinalRhapsody where
  getActions i window (FinalRhapsody attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env FinalRhapsody where
  runMessage msg t@(FinalRhapsody attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessages [RequestTokens (TreacherySource tid) iid 5 SetAside]
      FinalRhapsody <$> runMessage msg (attrs & resolved .~ False)
    RequestedTokens (TreacherySource tid) iid tokens | tid == treacheryId -> do
      let
        damageCount =
          count (\token -> token == Skull || token == AutoFail) tokens
      t <$ unshiftMessages
        [ InvestigatorAssignDamage
          iid
          (TreacherySource treacheryId)
          damageCount
          damageCount
        , ResetTokens
        , Discard (TreacheryTarget treacheryId)
        ]
    _ -> FinalRhapsody <$> runMessage msg attrs
