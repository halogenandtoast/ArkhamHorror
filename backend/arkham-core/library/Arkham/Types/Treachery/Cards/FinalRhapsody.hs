{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.FinalRhapsody where

import Arkham.Import

import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype FinalRhapsody = FinalRhapsody Attrs
  deriving newtype (Show, ToJSON, FromJSON)

finalRhapsody :: TreacheryId -> Maybe InvestigatorId -> FinalRhapsody
finalRhapsody uuid iid = FinalRhapsody $ weaknessAttrs uuid iid "02013"

instance HasModifiersFor env FinalRhapsody where
  getModifiersFor = noModifiersFor

instance HasActions env FinalRhapsody where
  getActions i window (FinalRhapsody attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env FinalRhapsody where
  runMessage msg t@(FinalRhapsody attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessages [RequestTokens source (Just iid) 5 SetAside]
      FinalRhapsody <$> runMessage msg (attrs & resolved .~ False)
    RequestedTokens source (Just iid) faces | isSource attrs source -> do
      let damageCount = count (`elem` [Skull, AutoFail]) faces
      t <$ unshiftMessages
        [ InvestigatorAssignDamage iid source damageCount damageCount
        , ResetTokens source
        , Discard (TreacheryTarget treacheryId)
        ]
    _ -> FinalRhapsody <$> runMessage msg attrs
