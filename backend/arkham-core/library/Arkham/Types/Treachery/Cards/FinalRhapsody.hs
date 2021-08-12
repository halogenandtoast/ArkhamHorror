module Arkham.Types.Treachery.Cards.FinalRhapsody where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Token
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype FinalRhapsody = FinalRhapsody TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finalRhapsody :: TreacheryCard FinalRhapsody
finalRhapsody = treachery FinalRhapsody Cards.finalRhapsody

instance TreacheryRunner env => RunMessage env FinalRhapsody where
  runMessage msg t@(FinalRhapsody attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ push (RequestTokens source (Just iid) 5 SetAside)
    RequestedTokens source (Just iid) tokens | isSource attrs source -> do
      let damageCount = count ((`elem` [Skull, AutoFail]) . tokenFace) tokens
      t <$ pushAll
        [ chooseOne iid [Continue ("Take " <> tshow damageCount <> " damage")]
        , InvestigatorAssignDamage iid source DamageAny damageCount damageCount
        , ResetTokens source
        , Discard $ toTarget attrs
        ]
    _ -> FinalRhapsody <$> runMessage msg attrs
