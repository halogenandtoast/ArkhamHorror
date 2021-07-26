module Arkham.Types.Effect.Effects.WilliamYorick
  ( WilliamYorick(..)
  , williamYorick
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype WilliamYorick = WilliamYorick EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamYorick :: EffectArgs -> WilliamYorick
williamYorick = WilliamYorick . uncurry4 (baseAttrs "03005")

instance HasModifiersFor env WilliamYorick

instance
  ( HasList DiscardedPlayerCard env InvestigatorId
  , HasQueue env
  )
  => RunMessage env WilliamYorick where
  runMessage msg e@(WilliamYorick attrs) = case msg of
    PassedSkillTest _ _ _ SkillTestInitiatorTarget{} _ _ ->
      case effectTarget attrs of
        InvestigatorTarget iid -> do
          discards <- map unDiscardedPlayerCard <$> getList iid
          e <$ push
            (chooseOne
              iid
              (Done "Do not return card to hand"
              : [ TargetLabel
                    (CardIdTarget $ toCardId card)
                    [AddToHand iid $ PlayerCard card]
                | card <- discards
                ]
              )
            )
        _ -> pure e
    SkillTestEnds _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> WilliamYorick <$> runMessage msg attrs
