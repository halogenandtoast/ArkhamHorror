module Arkham.Types.Treachery.Cards.FinalRhapsody where


import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype FinalRhapsody = FinalRhapsody TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finalRhapsody :: TreacheryId -> Maybe InvestigatorId -> FinalRhapsody
finalRhapsody uuid iid = FinalRhapsody $ weaknessAttrs uuid iid "02013"

instance HasModifiersFor env FinalRhapsody where
  getModifiersFor = noModifiersFor

instance HasActions env FinalRhapsody where
  getActions i window (FinalRhapsody attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env FinalRhapsody where
  runMessage msg t@(FinalRhapsody attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ unshiftMessage (RequestTokens source (Just iid) 5 SetAside)
    RequestedTokens source (Just iid) faces | isSource attrs source -> do
      let damageCount = count (`elem` [Skull, AutoFail]) faces
      t <$ unshiftMessages
        [ InvestigatorAssignDamage iid source DamageAny damageCount damageCount
        , ResetTokens source
        , Discard $ toTarget attrs
        ]
    _ -> FinalRhapsody <$> runMessage msg attrs
