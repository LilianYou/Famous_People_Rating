function draw_Confidence(D, output, stim, ques, name)

Screen('DrawLine', D.window, [150 150 150], D.screensize_c*0.19, D.screensize_r*0.89, D.screensize_c*0.9,D.screensize_r*0.89);
Screen('DrawText', D.window, output, D.screensize_c*0.10, D.screensize_r*0.838);
Screen('DrawText', D.window, ques, D.screensize_c*0.036, D.screensize_r*0.2);
Screen('DrawText', D.window, name, D.screensize_c*0.4, D.screensize_r*0.65);
Screen('DrawTexture',D.window,stim,[],D.rect_center); 

return;
